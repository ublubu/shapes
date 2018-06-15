- Generate contacts
  - Needs SHAPE & LABELS (incl staticness)
  - Produces CONTACTS

- Calculate constraints
  - Needs CONTACTS
  - Produces CONSTRAINTS
  
- Calculating and clamping constraint solutions
  - Needs CONSTRAINTS & SOLUTIONS & VELOCITY & MU & ELASTICITY
  - Modifies VELOCITY & SOLUTIONS

- Final update
  - Needs SHAPE & VELOCITY & POSITION
  - Modifies SHAPE & POSITION
  
- Object properties
  - Dynamic SHAPE & VELOCITY
  - Static MU & ELASTICITY


Current setup:
- Mutable unboxed vector:
  - SOLUTIONS
- Immutable unboxed vector:
  - CONSTRAINTS
- IntMap:
  - SHAPE & LABELS & VELOCITY & POSITION & MU & ELASTICITY
- List:
  - CONTACTS

Unboxed mutable vector world:
- Mutable unboxed vector:
  - SOLUTIONS
  - VELOCITY & POSITION
  - LABELS
  - MU & ELASTICITY
- Mutable boxed vector:
  - SHAPE
- Immutable unboxed vector:
  - CONSTRAINTS
- List:
  - CONTACTS

