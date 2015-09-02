module MiniYampa where

import Prelude

import Control.Arrow
import Control.Apply
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Rec.Class (tailRecM3)
import Data.Either
import Data.Maybe
import Data.Functor
import Data.Monoid
import Data.Tuple
import Data.AdditiveGroup
import Data.VectorSpace
import Data.Profunctor
import Data.Profunctor.Strong

data Event a = Event a | NoEvent

isEvent :: forall a. Event a -> Boolean
isEvent (Event _) = true
isEvent NoEvent   = false

instance functorEvent :: Functor Event where
  map f NoEvent   = NoEvent
  map f (Event x) = Event (f x)

maybeToEvent :: forall a. Maybe a -> Event a
maybeToEvent (Just a) = Event a
maybeToEvent Nothing  = NoEvent

tagWith :: forall a x. a -> Event x -> Event a
tagWith x = map (const x)

attach :: forall a b. Event a -> b -> Event (Tuple a b)
attach e b = e <#> \a -> Tuple a b

edge :: SF Boolean (Event Unit)
edge = SF (edgeSF true)
  where
    edgeSF prev dt cur = { sf: SF (edgeSF cur)
                         , output: (if not prev && cur then Event unit else NoEvent) }

edgeJust :: forall a. SF (Maybe a) (Event a)
edgeJust = SF (edgeJustSF true)
  where
    edgeJustSF _ dt Nothing = { sf: SF (edgeJustSF false), output: NoEvent }
    edgeJustSF prev dt (Just x) = { sf: SF (edgeJustSF true)
                                  , output: (if prev then NoEvent else Event x) }

edgeBy :: forall a b. (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy isEdge a0 = SF (edgeJustSF a0)
  where
    edgeJustSF a dt a' = { sf: SF (edgeJustSF a')
                         , output: maybeToEvent (isEdge a a') }

updated :: forall a. (Eq a) => SF a (Event a)
updated = Just ^>> edgeBy detector Nothing
  where detector a a' = if a /= a' then a' else Nothing

switch :: forall a b c. SF a (Tuple b (Event c)) -> (c -> SF a b) -> SF a b
switch (SF sf) k = SF dSwitchF
  where
    dSwitchF dt x = case sf dt x of
      { sf: sf, output: Tuple output NoEvent }
          -> { sf: dSwitch sf k, output: output }
      { output: Tuple output (Event e) }
          -> (runSF $ k e) dt x

dSwitch :: forall a b c. SF a (Tuple b (Event c)) -> (c -> SF a b) -> SF a b
dSwitch (SF sf) k = SF dSwitchF
  where
    dSwitchF dt x = case sf dt x of
      { sf: sf, output: Tuple output NoEvent }
          -> { sf: dSwitch sf k, output: output }
      { output: Tuple output (Event e) }
          -> { sf: k e, output: output }

kSwitch :: forall a b c. SF a b -> SF (Tuple a b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
kSwitch (SF sf0) raiseSF@(SF raiseF) k = SF (switchF sf0)
  where
    switchF sf dt x = case rr.output of
        NoEvent -> { sf: kSwitch r.sf raiseSF k, output: r.output }
        Event e -> (runSF $ k (SF sf) e) dt x
      where
        r = sf dt x
        rr = raiseF dt $ Tuple x r.output

accumHoldBy :: forall a b. (b -> a -> b) -> b -> SF (Event a) b
accumHoldBy op init = SF (accumHoldBySF init)
  where
    accumHoldBySF s dt NoEvent   = { sf: SF (accumHoldBySF s), output: s }
    accumHoldBySF s dt (Event x) = { sf: SF (accumHoldBySF sNext), output: sNext }
      where sNext = op s x

integral :: forall a. (AdditiveGroup a, VectorSpace a Number) => SF a a
integral = imIntegral zeroV

imIntegral :: forall a. (AdditiveGroup a, VectorSpace a Number) => a -> SF a a
imIntegral init = SF (integralF init)
  where
    integralF acc dt x = { sf: SF (integralF res), output: acc }
      where
          res = acc ^+^ (x ^* dt)

time :: forall a. SF a Time
time = constant 1.0 >>> integral

type Time = Number
type DTime = Number

newtype SF a b = SF (DTime -> a -> { sf :: (SF a b), output :: b })

runSF :: forall a b. SF a b -> DTime -> a -> { sf :: SF a b, output :: b }
runSF (SF sf) = sf

instance semigroupoidSF :: Semigroupoid SF where
  compose (SF sf2) (SF sf1) = SF sf12
    where
      sf12 dt x1 = { sf: (compose r2.sf r1.sf), output: r2.output }
        where
          r1 = sf1 dt x1
          r2 = sf2 dt r1.output

instance categorySF :: Category SF where
  id = idSF
    where idSF = SF (\dt x -> { sf: idSF, output: x })

instance profunctorSF :: Profunctor SF where
  dimap a2b c2d b2c = arrSF a2b >>> b2c >>> arrSF c2d

arrSF :: forall a b. (a -> b) -> SF a b
arrSF f = arrSF'
  where arrSF' = SF (\dt x -> { sf: arrSF', output: (f x) })

instance strongSF :: Strong SF where
  first = firstSF
  second = dimap swap swap <<< firstSF

firstSF :: forall a b c. SF a b -> SF (Tuple a c) (Tuple b c)
firstSF (SF sf) = SF firstF
  where
    firstF dt (Tuple x y) = { sf: (firstSF r.sf), output: (Tuple r.output y) }
      where
        r = sf dt x

instance arrowSF :: Arrow SF

identity :: forall a. SF a a
identity = arr id

constant :: forall a b. b -> SF a b
constant x = arr (\_ -> x)

reactimate :: forall eff a b.
              Eff eff a
           -> (Boolean -> Eff eff (Tuple DTime (Maybe a)))
           -> (Boolean -> b -> Eff eff Boolean)
           -> SF a b
           -> Eff eff Unit
reactimate init sense actuate (SF sf0) = do
  a0 <- init
  let r = sf0 0.0 a0
  tailRecM3 go r.sf a0 r.output
  where
    go (SF sf) a b = do
      done <- actuate true b
      if done
        then pure $ Right unit
        else do
           (Tuple dt ma') <- sense false
           let a' = fromMaybe a ma'
               r2 = sf dt a'
           pure $ Left { a: r2.sf, b: a', c: r2.output }

newtype ReactState a b eff
  = ReactState { actuate :: ReactHandle a b eff -> Boolean -> b -> Eff eff Boolean
               , sf :: SF a b
               , a :: a
               , b :: b
               }

type ReactHandle a b eff = Ref (ReactState a b eff)

reactInit :: forall a b eff.
             Eff (ref :: REF | eff) a
          -> (ReactHandle a b (ref :: REF | eff) -> Boolean -> b -> Eff (ref :: REF | eff) Boolean)
          -> SF a b
          -> Eff (ref :: REF | eff) (ReactHandle a b (ref :: REF | eff))
reactInit init actuate (SF sf0) = do
  a0 <- init
  let r0 = sf0 0.0 a0
  r <- newRef (ReactState { actuate: actuate, sf: r0.sf, a: a0, b: r0.output })
  actuate r true r0.output
  pure r

react :: forall a b eff.
         ReactHandle a b (ref :: REF | eff)
      -> Tuple DTime (Maybe a)
      -> Eff (ref :: REF | eff) Boolean
react rh (Tuple dt ma') = do
  (ReactState rs) <- readRef rh
  let a' = fromMaybe rs.a ma'
      r = (runSF rs.sf) dt a'
  writeRef rh (ReactState rs { sf = r.sf, a = a', b = r.output })
  done <- rs.actuate rh true r.output
  pure done

infixr 1 >>^
(>>^) :: forall a b c. SF a b -> (b -> c) -> SF a c
(>>^) x y = x >>> arr y

infixr 1 ^>>
(^>>) :: forall a b c. (a -> b) -> SF b c -> SF a c
(^>>) x y = arr x >>> y

dup a = Tuple a a
