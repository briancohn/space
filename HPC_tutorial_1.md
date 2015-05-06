 Portable batch scheduler
	 pbsdsh



```bash
#!/bin/bash
#PBS -l walltime=24:00:00
#PBS -l nodes=2:ppn=16
```
The ppn means the number of processes per node.

Most nodes have 8 or 16 cores.
Choose 16 cores to get the newer cores.


You can also let it figure out the processes for you.
```bash
#PBS -l procs=100
```


```bash
pbsdsh /hom
