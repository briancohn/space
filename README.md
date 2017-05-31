# space
[![Build Status](https://travis-ci.org/bcohn12/space.svg?branch=master)](https://travis-ci.org/bcohn12/space)
[![Build Status](https://semaphoreci.com/api/v1/projects/a45fbc4a-ee80-4f0c-91c5-61ddffa1cad9/416002/badge.svg)](https://semaphoreci.com/bcohn12/space)      
[![Codacy Badge](https://www.codacy.com/project/badge/df8cbf6a225249ae9619b25cc613f0e8)](https://www.codacy.com/app/brian_cohn14/space)

[<img src = "https://cloud.githubusercontent.com/assets/4623063/26612928/d8aa76c0-456c-11e7-9ba9-0ca74b01049b.gif">](https://briancohn.github.io/space-parcoords/)  

Code and Supplementary Material for computing and visualizing the feasible activation space—the way that muscles combine to create forces at the end of a limb.

#### Problem statement:
We don't know how the brain picks the <strong>optimal</strong> strategy to coordinate its muscles and limbs to generate motion.
#### Why we are researching this:
If we understand the constraints of force generation, we can see how many solutions the brain can choose from. Prosthetic and robotic designs are dependent upon robust control algorithms;  we have the opportunity to unveil how the brain optimizes its coordination strategy, and build the next generation of biologically-inspired robot arms, prosthetic designs, and orthopaedic treatments.
#### How do we approach this topic:
Take in data about a musculoskeletal arm or leg, and visualize how the brain optimizes the limb's force production. With many different muscles, there is an infinite number of control strategies. In this project, we look to constrain those possibilties as much as possible, until we uncover physical limitations to the solution space itself.

#### How are we doing this?
An N-Dimensional system of joints and muscles can get complex quickly. We look to approximate the volume of the solution subspace using custom random number generation and an analysis of the solutions we find.

# Parallel Coordinates Visualization:
[Link](https://briancohn.github.io/space-parcoords/)

#### What tools will help us solve this problem:
This project can get computationally complex, very quickly. We have a cat-leg model that's got 31 muscles, 7 Degrees of freedom (at the joints), and 6 output dimensions (x,y,z,t<sub>x</sub>,t<sub>y</sub>,t<sub>z</sub>).

Imagine you're holding a brick in your palm that weighs 2kg, and you can't move your elbow or shoulder. You have to push up with 20N force or the brick will move. It will require a direct upward force, and you can't create any torque on the brick (you're not allowed to spin it).
<img src="http://www.beldenbrick.com/2007/images/modular.gif" alt="picture of a brick">

The brick isn't heavy for you, and because it's not your 'maximal', there are many ways to create that force directly up. Your arm has many different muscles which can support the brick. Some muscles are absolutely necessary, while others you could inject with botox, and still maintain effective control of the brick (albeit it might be more difficult). Every muscle you have can be tensed from 0 to 100%, where 100% is as hard as you can possibly contract that muscle. Your nervous system controls the 'activations' of these muscles, thereby producing forces and controlling motion for your body. Every moment of your life, your arm muscles have a level of activation. When sleeping, you might have very low muscle tension, but when holding the brick, some of the muscles are more highly activated.

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


# The big questions:
When you have `n` muscles controlling the force at an endpoint, and you want to generate an exact force of `10N` directly up, what are all of the possible activation sets?


# Installation of Scala and SBT
```bash
sudo apt-get update
sudo apt-get install scala
wget https://dl.bintray.com/sbt/debian/sbt-0.13.7.deb
sudo apt-get update
sudo dpkg -i sbt-0.13.7.deb
```
# Running the project
```bash
 cd ~/your/path/to/space
 mkdir output
 sbt run 100
```
This will generate 100 uniformly distributed points within a 7-muscle index finger model, and put the results CSVs in the output folder.

# Mapping Code
```latex
J^{-T} \in \mathbb{R}^{m\times k} ,
R \in \mathbb{R}^{k\times n} ,
F_O \in \mathbb{R}^{n\times n} ,
\textbf{a} \in \mathbb{R}^n\\

m=\text{Output Dimensions}\\
k=\text{DOF}\\
n=\text{Muscles}\\
```
