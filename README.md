# space
[![Build Status](https://travis-ci.org/bcohn12/space.svg?branch=master)](https://travis-ci.org/bcohn12/space)
####Problem statement:
We don't know how the brain picks the <strong>optimal</strong> strategy to coordinate its muscles and limbs to generate motion.
####Why we are researching this:
If we understand the constraints of force generation, we can see how many solutions the brain can choose from. Prosthetic and robotic designs are dependent upon robust control algorithms; we have the opportunity to unveil how the brain optimizes its coordination strategy, and build the next generation of biologically-inspired robot arms, prosthetic designs, and orthopaedic treatments.
####How do we approach this topic:
Take in data about a musculoskeletal arm or leg, and visualize how the brain optimizes the limb's force production. With many different muscles, there is an infinite number of control strategies. In this project, we look to constrain those possibilties as much as possible, until we uncover physical limitations to the solution space itself.

####How are we doing this?
An N-Dimensional system of joints and muscles can get complex quickly. We look to approximate the volume of the solution subspace using custom random number generation and an analysis of the solutions we find.
####What tools will help us solve this problem:
This project can get computationally complex, very quickly. We have a cat-leg model that's got 31 muscles, 7 Degrees of freedom (at the joints), and 6 output dimensions (x,y,z,t<sub>x</sub>,t<sub>y</sub>,t<sub>z</sub>).

Imagine you're holding a brick in your palm that weighs 2kg, and you can't move your elbow or shoulder. You have to push up with 20N force or the brick will move. It will require a direct upward force, and you can't create any torque on the brick (you're not allowed to spin it).
<img src="http://www.beldenbrick.com/2007/images/modular.gif" alt="picture of a brick">

The brick isn't heavy for you, and because it's not your 'maximal' there's alot of ways you could create that force directly up. Your arm has many different muscles which can support the brick. Some muscles are absolutely necessary, while others you could inject with botox, and still maintain effective control of the brick (albeit it might be more difficult). Every muscle you have can be tensed from 0 to 100%, where 100% is as hard as you can possibly contract that muscle. Your nervous system controls the 'activations' of these muscles, thereby producing forces and controlling motion for your body. Every moment of your life, your arm muscles have a level of activation. When sleeping, you might have very low muscle tension, but when holding the brick, some of the muscles are more highly activated.

##a
Think of your muscle activations as a list of percentages:

muscle  | activation
------------- | -------------
m <sub>0</sub>  | 30% 
m <sub>1</sub>  | 20%
m <sub>2</sub>  | 100%
...       	    | ...
m <sub>n</sub>  | a<sub>n</sub> %

We represent this as a set of these activations, where activation is between 0 (0%) and 1 (100%).
<strong>a</strong> = (a<sub>0</sub> ,a<sub>1</sub> ,a<sub>2</sub> , ... , a<sub>n</sub> )

###Example
if you had 3 muscles controlling the 20N force upon the brick, your activation set could be any of these combinations:
<strong>a</strong> = `(0.1, 0.5, 0)`
<strong>a</strong> = `(0.5, 0.8, 0)`
<strong>a</strong> = `(0.6, 0.9, 0.1)`
	These are all feasible <i>solutions</i>, in that they can all generate the exact same output force in the same direction.


#The big questions:
When you have `n` muscles controlling the force at an endpoint, and you want to generate an exact force of `10N` directly up, what are all of the possible activation sets?
###Visualization with a Feasible Activation Set (FAS)
What does the 'solution space' look like?

Every point within the Feasible Activation Set will produce the desired force output. The dimensionality of the FAS is highly dependent on the number of muscles and whether there are any other constraints on the system (to be discussed in another post).
##Example
If you had 4 muscles in your system, the feasible activaiton space could look like this:
<img src="http://upload.wikimedia.org/wikipedia/commons/e/ef/3dpoly.svg">



#Where do most of the solutions lie?
TODO: add a figure describing how projections of density onto one dimension can work with volume, use a triangle or a pyramid for 2d and 3d examples.



#What are the connections between the muscle activations?
TODO: look at how solution vectors change when one muscle is constrained.
Are there patterns?
Are there inverse relationships?
#Prediction
Can I predict how much muscle 0 will be activated, if I know muscles 1 through n?
TODO: look into Linear and Nonlinear prediction schemes
TODO: Look into research on prosthetic muscle control systems (e.g. can we build a model to predict muscle activation via the other muscles?)

#Probabalistic Heat Maps
TODO: develop figures

#Talk about assumptions
Muscles insertions, strengths, joint movement planes, Output XYZ dimensions, Nonlinearities

#Implementation
Make a program to extract uniformly-distributed points within the n dimensional feasible activation space.
TODO: mathematical formula for this recursive function
Multiple cores will contribute their random points to a unified database with all solutions thus far.
We will generate queries with constraints upon one or more muscles, to identify how other muscle compensate for constraints (<i>i.e.</i> loss of a muscle's function, weakening of a muscle, or strengthening of a muscle.)
Query example:
"How does muscle 1 activate when muscle 0's activation is limited to 50%?"
`SELECT m1 FROM POINTS_TABLE WHERE m0 < 0.5`