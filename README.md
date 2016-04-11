# shapes #

physics engine and other tools for 2D shapes

https://youtu.be/DYzf4zBK90o?list=PLmozfF6FosKjmPMnlPoVbWosiExbD0SUF

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
  
  **Note**: Solving a constraint affects the velocity of the object(s).
