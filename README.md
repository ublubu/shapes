# shapes #

physics engine and other tools for 2D shapes

https://youtu.be/j1Abxcc86yU?list=PLmozfF6FosKjmPMnlPoVbWosiExbD0SUF

## About ##

`shapes` started out as my second Haskell project. I'm still working on it. It needs to be faster.

## Libraries ##
  
  * **`shapes-math`**: TemplateHaskell-generated vector math using GHC primops (in place of `linear`)
  * **`shapes`**: physics engine implementation
  * **`shapes-demo`**: interactive demo and debugging tool for `shapes`

## Engine Components ##

  * **Contact**: find contacts between shapes - e.g. Separating Axis Test (`SAT`)
  * **Constraint**: general form (and solution) for constraints on motion of objects
  * **Solver**: solving the set of all constraints in a world
  * **Broadphase**: finding potential interactions (that would give rise to constraints)

### How the Solver works ###
  start at `Physics.Engine.Opt.Main`

  1. Broadphase: Compare axis-aligned bounding boxes (`Aabb`) to find pairs of objects that may be interacting. Feed these pairs into the solver.
  2. Generate constraint generators. 

    * Calculate contacts between a pair of objects (`SAT`).
    * Create generators for relevant constraints at each contact (e.g. `Friction`, `NonPenetration`)
    * Constraint generators are evaluated with the objects' current state (see 4, Note).

  3. For each constraint generator that also existed in the previous frame,
     apply the previous frame's constraint solution.
  4. Evaluate and solve each constraint generator in sequence. (Optional: repeat this step)
  
  **Note**: The solution to a constraint is a change in velocity.

## `Opt` vs `Simple` ##

The `Simple` modules serve as a reference implementation. I'll use it to make sure my optimizations don't break `Opt`.

I only recently started maintaining a reference implementation, so `Simple` actually contains several optimizations.
I plan to resimplify `Simple` so it can better serve its purpose.
