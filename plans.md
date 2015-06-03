A = 
-0.35  1.0   0.0   0.0   0.0   0.0   
0.35   -1.0  -0.0  -0.0  -0.0  -0.0  
0.0    0.0   -4.0  1.0   0.0   0.0   
-0.0   -0.0  4.0   -1.0  -0.0  -0.0  
0.0    0.0   0.0   0.0   -0.7  1.0   
-0.0   -0.0  -0.0  -0.0  0.7   -1.0  
-1.0   -0.0  -0.0  -0.0  -0.0  -0.0  
-0.0   -1.0  -0.0  -0.0  -0.0  -0.0  
-0.0   -0.0  -1.0  -0.0  -0.0  -0.0  
-0.0   -0.0  -0.0  -1.0  -0.0  -0.0  
-0.0   -0.0  -0.0  -0.0  -1.0  -0.0  
-0.0   -0.0  -0.0  -0.0  -0.0  -1.0  
1.0    0.0   0.0   0.0   0.0   0.0   
0.0    1.0   0.0   0.0   0.0   0.0   
0.0    0.0   1.0   0.0   0.0   0.0   
0.0    0.0   0.0   1.0   0.0   0.0   
0.0    0.0   0.0   0.0   1.0   0.0   
0.0    0.0   0.0   0.0   0.0   1.0   
1.0    0.0   -1.0  0.0   0.0   0.0   
0.0    1.0   0.0   -1.0  0.0   0.0   
0.0    0.0   1.0   0.0   -1.0  0.0   
0.0    0.0   0.0   1.0   0.0   -1.0  
-1.0   -0.0  1.0   -0.0  -0.0  -0.0  
-0.0   -1.0  -0.0  1.0   -0.0  -0.0  
-0.0   -0.0  -1.0  -0.0  1.0   -0.0  
-0.0   -0.0  -0.0  -1.0  -0.0  1.0  

b = 
DenseVector(0.2, -0.2, -2.0, 2.0, 0.3, -0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)

add on Aprime and bprime for the following cases.


where p= {p1,p2} at moment j
------------------------------------------------------
in this case j=k
where looking at the last point j=k
assert isInFeasibleTrajectories==True for  p= [0.6,0.72]
assert isInFeasibleTrajectories==False for p= [0.0,0.3]
0 0 0 0 1   0  b=p1
0 0 0 0 0   1  b=p2
0 0 0 0 -1  0  b=-p1
0 0 0 0 0  -1  b=-p2

--------------------------------------------
assert isInFeasibleTrajectories==True for p= [44/73, 30/73]
assert isInFeasibleTrajectories==False for p= [0.0, 0.2]
If j=0 looking at the first case
1  0 0 0 0 0  b=p1
0  1 0 0 0 0  b=p2
-1 0 0 0 0 0 b=-p1
0 -1 0 0 0 0 b=-p2
-----------------------------------------------------
assert isInFeasibleTrajectories==True for p= [44/73, 30/73]
assert isInFeasibleTrajectories==False for p= [0.5, 0.0]
if j=1
0 0 1 0 0 0  b=p1
0 0 0 1 0 0  b=p2
0 0 -1 0 0 0 b=-p1
0 0 0 -1 0 0 b=-p2


Once you make the matrix,
run it through Linearprogram(A,B,c = DenseVector.ones[Double](n*K))

----------------------------------------------------------------------
======================================================================

THEN 

Do hit and run on timepoint j=s
collect all of the points  in a matrix P
val P = HitRun(A,b)
c = all ones of len k*n
val TruePoints = P.filter(x => LinProg(FixVars(x,j=s,A,n,K),b,c))



input: KGeneratorSystems
output: Histograms of past present and future and both