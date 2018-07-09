{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad ( zipWithM_ )
import Data.Foldable ( toList )
import Data.Monoid ( Monoid(..) )
import Data.SBV
import Data.SBV.Tools.CodeGen
import Data.SBV.Internals ( SolverContext )
import Data.Semigroup ( Semigroup(..) )

import qualified Numeric.GSL.ODE as ODE
import qualified Numeric.LinearAlgebra as LA

import Graphics.Rendering.Chart.Easy hiding ( (.>) )
import Graphics.Rendering.Chart.Backend.Cairo

data Pendulum a = Pendulum
  { pendulumLength :: a
  , pendulumDampingConstant :: a
  , pendulumMass :: a
  , pendulumGravity :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data State a = State
  { stateθ :: a
  , stateω :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Controller a = Controller
  { controllerDamping :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance EqSymbolic a => EqSymbolic (State a) where
  (State x a) .== (State y b) = x .== y &&& a .== b

stateLabels :: State String
stateLabels = State "θ" "ω"

pendulum :: Fractional a => Pendulum a -> a -> State a -> State a
pendulum sys@(Pendulum len damping _ grav) τ (State θ ω) =
  State ω $
  (grav * taylorSin θ) / len + (damping * ω) / inertia + τ / inertia
  where
    inertia = pendulumInertia sys

pendulumInertia :: Fractional a => Pendulum a -> a
pendulumInertia (Pendulum len _ mass _) = mass * len * len

kineticEnergy :: Fractional a => Pendulum a -> State a -> a
kineticEnergy system (State _ ω) =
  0.5 * pendulumInertia system * ω * ω

dkedt ::
  Fractional a => Controller a -> Pendulum a -> State a -> a
dkedt ctrl system state@(State _ ω) =
  pendulumInertia system * ω * stateω (closedLoop ctrl system state)

-- | Potential energy spans [-2, 0] * mg.
potentialEnergy :: Fractional a => Pendulum a -> State a -> a
potentialEnergy (Pendulum len _ mass grav) (State θ _) =
  len * mass * grav * (taylorCos θ - 1)

dpedt :: Fractional a => Pendulum a -> State a -> a
dpedt (Pendulum len _ mass grav) (State θ ω) =
  len * mass * grav * (- taylorSin θ) * ω

lyapunovController :: Fractional a => Controller a -> Pendulum a -> State a -> a
lyapunovController (Controller kd) (Pendulum len _ mass grav) (State θ ω) =
  (-2) * mass * grav * len * taylorSin θ - kd * ω

closedLoop ::
  Fractional a => Controller a -> Pendulum a -> State a -> State a
closedLoop ctrl system initialState =
  pendulum system torque initialState
  where
    torque = lyapunovController ctrl system initialState

{- Proofs -}

v :: Fractional a => Pendulum a -> State a -> a
v system st =
  kineticEnergy system st - potentialEnergy system st

dvdt :: Fractional a => Controller a -> Pendulum a -> State a -> a
dvdt ctrl system st =
  dkedt ctrl system st - dpedt system st

newtype SAll a = SAll { getSAll :: a }

instance Boolean a => Semigroup (SAll a) where
  (SAll x) <> (SAll y) = SAll $ x &&& y

instance Boolean a => Monoid (SAll a) where
  mempty = SAll true
  mappend = (<>)

allIsPoint :: (IEEEFloating a, Foldable t) => t (SBV a) -> SBool
allIsPoint = getSAll . foldMap (SAll . fpIsPoint)

nanFree ::
  (IEEEFloating a, SolverContext m, Monad m) =>
  (String -> m (SBV a)) -> (State (SBV a) -> SBV a) -> m SBool
nanFree gen f = do
  st <- traverse gen stateLabels
  constrainPi st
  constrainFP st
  pure . fpIsPoint $ f st
  where
    constrainFP = constrain . allIsPoint
    constrainPi (State θ _) = constrain $ θ .<= π &&& θ .> -π
    π = 3.1415926535897932384626433832795028841971693993751

lyapunov'sTheorem ::
  ( SymWord a, Fractional a
  , SolverContext m, Monad m) =>
  (String -> m (SBV a)) -> (State (SBV a) -> SBV a) -> (State (SBV a) -> SBV a) -> m SBool
lyapunov'sTheorem gen f dfdt = do
  st <- traverse gen stateLabels
  constrainPi st
--   constrainFP st
--   constrainFP [f st, dfdt st]
  eq <- lyapunovEquilibrium st
  nn <- lyapunovNonNegative st
  gn <- lyapunovGradNegative st
  pure $ eq &&& nn &&& gn
  where
    constrainPi (State θ _) = constrain $ θ .<= π &&& θ .> -π
    π = 3.1415926535897932384626433832795028841971693993751
    lyapunovEquilibrium _ = pure $
      f (State 0 0) .== 0.0

    lyapunovNonNegative st = do
      constrain $ st ./= State 0 0
      pure $ f st .> 0.0

    lyapunovGradNegative st = pure $
      dfdt st .<= 0.0 &&& dfdt (State 0 0) .<= 0.0

nominalController :: Fractional a => Controller a
nominalController = Controller 0.3

nominalSystem :: Fractional a => Pendulum a
nominalSystem = Pendulum 0.5 (-0.03) 0.1 9.81

systemLabels :: Pendulum String
systemLabels = Pendulum "length" "damping" "mass" "gravity"

controllerLabels :: Controller String
controllerLabels = Controller "kd"

proveStability :: IO ThmResult
proveStability =
  prove $ lyapunov'sTheorem sReal v' dvdt'
  where
    v' = v nominalSystem
    dvdt' = dvdt nominalController nominalSystem

proveNanSafety :: IO ThmResult
proveNanSafety =
  prove $ nanFree sFloat controller
  where
    controller = lyapunovController nominalController nominalSystem

-- Simulation

dxdt :: Floating a => p -> [a] -> [a]
dxdt _ [θ, ω] = toList $
  closedLoop nominalController nominalSystem
  (State θ ω)
dxdt _ _ = error "Invalid arguments to 'dxdt'"

dxdtOpenLoop :: Fractional a => p -> [a] -> [a]
dxdtOpenLoop _ [θ, ω] =
  toList $ pendulum nominalSystem 0 (State θ ω)
dxdtOpenLoop _ _ = error "Invalid arguments to 'dxdtOpenLoop'"

solution :: State Double
         -> (Double -> [Double] -> [Double])
         -> LA.Vector Double
         -> LA.Matrix Double
solution state0 f = ODE.odeSolve f $ toList state0

listSolution :: State Double
             -> (Double -> [Double] -> [Double])
             -> [Double]
             -> [(Double, Double)]
listSolution state0 f =
  fmap assoc . LA.toRows . solution state0 f . LA.fromList
  where
    assoc vec = let [t, x] = LA.toList vec in (t, x)

initialStates :: Fractional a => [State a]
initialStates = zipWith State [1e-3, -0.5, 0.3] [1e-3, 0.1, 0.3]

sampleTs :: (Enum a, Fractional a) => [a]
sampleTs = [0, 0.01 .. 7]

makePlot :: PlotValue a
         => String -> String -> String -> [[(a, a)]] -> IO ()
makePlot nm title lbl trajectories =
  toFile opts (nm <> ".png") $ do
  layout_title .= title
  setColors $ fmap opaque [red, blue, green]
  zipWithM_ mkPlot [0 :: Int ..] trajectories
  where
    mkPlot num = plot . line (lbl <> show num) . pure
    opts = def { _fo_size = (1280, 720) }

plotStates :: String -> (Double -> [Double] -> [Double]) -> IO ()
plotStates prefix dynamics = do
  makePlot (prefix <> "_theta") (prefix <> " pendulum angle") "θ" thetas
  makePlot (prefix <> "_omega") (prefix <> " pendulum velocity") "ω" omegas
  where
    trajs = fmap (\st -> listSolution st dynamics sampleTs) initialStates
    withTime sel = zip sampleTs . fmap sel
    thetas, omegas :: [[(Double, Double)]]
    thetas = fmap (withTime fst) trajs
    omegas = fmap (withTime snd) trajs

unstabilized :: IO ()
unstabilized = plotStates "Unstabilized" dxdtOpenLoop

stabilized :: IO ()
stabilized = plotStates "Stabilized" dxdt

main :: IO ()
main = do
  unstabilized
  stabilized
  genCCode

-- Trigonometry

taylorCos :: Fractional a => a -> a
taylorCos x = 1 + sum (take 10 series)
  where
    inc num old =
      let new = old * x * x / (num * (num + 1))
      in new : inc (num + 2) new
    signs = cycle [negate, id]
    series = zipWith ($) signs (inc 1 1)

taylorSin :: Fractional a => a -> a
taylorSin x = x + sum (take 10 series)
  where
    inc num old =
      let new = old * x * x / (num * (num + 1))
      in new : inc (num + 2) new
    signs = cycle [negate, id]
    series = zipWith ($) signs (inc 2 x)

-- C code generation

emitController ::
  (Fractional a, SymWord a) =>
  (String -> SBVCodeGen (SBV a)) -> IO ()
emitController gen = compileToC Nothing "lyapunovController" $ do
  system <- traverse gen systemLabels
  controller <- traverse gen controllerLabels
  state <- traverse gen stateLabels
  cgReturn $ lyapunovController controller system state

genCCode :: IO ()
genCCode = do
  emitController cgGen
  emitTaylor taylorSin "taylorSin" cgGen
  emitTaylor taylorCos "taylorCos" cgGen
  emitCalloutController cgGen
  where
    cgGen :: String -> SBVCodeGen SDouble
    cgGen = cgInput

emitTaylor
  :: (Fractional a, SymWord a)
  => (SBV a -> SBV a)
  -> String
  -> (String -> SBVCodeGen (SBV a))
  -> IO ()
emitTaylor f funName gen = compileToC Nothing funName $
  gen "x" >>= cgReturn . f

taylorSin' :: (Fractional a, SymWord a) => SBV a -> SBV a
taylorSin' = cgUninterpret "taylorSin" mempty taylorSin

taylorCos' :: (Fractional a, SymWord a) => SBV a -> SBV a
taylorCos' = cgUninterpret "taylorCos" mempty taylorCos

lyapunovController'
  :: (SymWord a, Fractional a) =>
     Controller (SBV a)
     -> Pendulum (SBV a) -> State (SBV a) -> SBV a
lyapunovController' (Controller kd) (Pendulum len _ mass grav) (State θ ω) =
  -2 * mass * grav * len * taylorSin' θ + kd * (-ω)

emitCalloutController
  :: (SymWord a, Fractional a) =>
     (String -> SBVCodeGen (SBV a)) -> IO ()
emitCalloutController gen = compileToC Nothing "lyapunovController2" $ do
  system <- traverse gen systemLabels
  controller <- traverse gen controllerLabels
  state <- traverse gen stateLabels
  cgReturn $ lyapunovController' controller system state
