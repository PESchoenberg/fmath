#!/bin/bash
clear
echo 'Compiling fmath libraries...'
echo 'fmath3...'
gfortran -std='gnu' -c fmath3.f95 -O3 -march=native -Wall -Wextra -ffree-form
echo 'fmath4...'
gfortran -std='gnu' -c fmath4.f95 -O3 -march=native -Wall -Wextra -ffree-form
echo 'fmath1...'
gfortran -std='gnu' -c fmath1.f95 -O3 -march=native -Wall -Wextra -ffree-form
echo 'fmath2...'
gfortran -std='gnu' -c fmath2.f95 -O3 -march=native -Wall -Wextra -ffree-form
echo 'fmath5...'
gfortran -std='gnu' -c fmath5.f95 -O3 -march=native -Wall -Wextra -ffree-form
echo
echo 'Finished compiling fmath libraries.'

