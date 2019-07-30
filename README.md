Programming language: This game is coded completely using Racket and it's libraries

DESCRIPTION:
Our project presents a typical racing game, in which player (red car)
races against a computerized car( green car). The car accelerates up to a
maximum speed and also collide as per the law of physics(to best
possible extent). There are three tracks to race on.
Cars are bound to move on track, their speed goes to zero as soon as
they hit the boundary(head-on).
On collision, both car suffer impact distorting from their paths for some
time.
Race ends whenever one of the car crosses finish line.

INPUT/OUTPUT:
The car will be controlled by arrow keys(up, down, left, right).
While up and down keys accelerate and decelerate the car,
right and left keys rotate the direction of motion.

IMPLEMENTATION IDEA:
1. We treat car as a rectangle and keep track of it using its center and
orientation (dimensions previously known)
2. We keep measuring distances of 4 vertices of car(treated as a
rectangle) from boundary of track , so if it comes close enough to a
boundary the car stops
3. If any one bounding edge of car touches other it is considered as a
collision and speed of the car and direction are appropriately changed
4. If computerized car hits a boundary it turns only in a specific direction
depending upon weather it is inner boundary or outer boundary so that
it always moves in correct direction

LIMITATIONS AND BUGS:
1. Due to limitation of racket, it can’t take two inputs simultaneously so
we can’t rotate and accelerate at the same time .
2. Due to same reason multi player game cannot be created.

POINTS OF INTEREST:
1. The computerized car tries to avoid collision with the user car.If the
user car is in front of the computerized car the the computerized car
itself rotates and moves on a path on which user car will not be in front
of it.
2. During collision between the 2 cars sound is produced. If after the
collision orientation of the computerized car changes or becomes
opposite of initial orientation, then it detects it and changes its
orientation accordingly.
3. The sound produced during the starting of the game brings the
required liveliness in the game. The algorithm of the computerized car is
track independent
4. If we win it outputs our time of completion of the track, which makes
the game challenging.

PACKAGES USED:
htdp/image
htdp/universe
rsound
