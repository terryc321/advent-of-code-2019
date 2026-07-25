#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
// types 
static int64_t count = 0LL ;
static int64_t px1 = 0LL ;
static int64_t py1 = 0LL ;
static int64_t pz1 = 0LL ;
static int64_t vx1 = 0LL ;
static int64_t vy1 = 0LL ;
static int64_t vz1 = 0LL ;
static int64_t px2 = 0LL ;
static int64_t py2 = 0LL ;
static int64_t pz2 = 0LL ;
static int64_t vx2 = 0LL ;
static int64_t vy2 = 0LL ;
static int64_t vz2 = 0LL ;
static int64_t px3 = 0LL ;
static int64_t py3 = 0LL ;
static int64_t pz3 = 0LL ;
static int64_t vx3 = 0LL ;
static int64_t vy3 = 0LL ;
static int64_t vz3 = 0LL ;
static int64_t px4 = 0LL ;
static int64_t py4 = 0LL ;
static int64_t pz4 = 0LL ;
static int64_t vx4 = 0LL ;
static int64_t vy4 = 0LL ;
static int64_t vz4 = 0LL ;

int main(){
  px1 = -1LL ; py1 = 0LL ; pz1 = 2LL ;
  px2 = 2LL ; py2 = -10LL ; pz2 = -7LL ;
  px3 = 4LL ; py3 = -8LL ; pz3 = 8LL ;
  px4 = 3LL ; py4 = 5LL ; pz4 = -1LL ;
  while (1) {
    // moons 1 - 2 
    // x 
    // x axis 
    if (px1 < px2) { vx1++; vx2--; } 
    if (px2 < px1) { vx2++; vx1--; } 
    // y axis 
    if (py1 < py2) { vy1++; vy2--; } 
    if (py2 < py1) { vy2++; vy1--; } 
    // z axis 
    if (pz1 < pz2) { vz1++; vz2--; } 
    if (pz2 < pz1) { vz2++; vz1--; } 
    // moons 1 - 3 
    // x 
    // x axis 
    if (px1 < px3) { vx1++; vx3--; } 
    if (px3 < px1) { vx3++; vx1--; } 
    // y axis 
    if (py1 < py3) { vy1++; vy3--; } 
    if (py3 < py1) { vy3++; vy1--; } 
    // z axis 
    if (pz1 < pz3) { vz1++; vz3--; } 
    if (pz3 < pz1) { vz3++; vz1--; } 
    // moons 1 - 4 
    // x 
    // x axis 
    if (px1 < px4) { vx1++; vx4--; } 
    if (px4 < px1) { vx4++; vx1--; } 
    // y axis 
    if (py1 < py4) { vy1++; vy4--; } 
    if (py4 < py1) { vy4++; vy1--; } 
    // z axis 
    if (pz1 < pz4) { vz1++; vz4--; } 
    if (pz4 < pz1) { vz4++; vz1--; } 
    // moons 2 - 3 
    // x 
    // x axis 
    if (px2 < px3) { vx2++; vx3--; } 
    if (px3 < px2) { vx3++; vx2--; } 
    // y axis 
    if (py2 < py3) { vy2++; vy3--; } 
    if (py3 < py2) { vy3++; vy2--; } 
    // z axis 
    if (pz2 < pz3) { vz2++; vz3--; } 
    if (pz3 < pz2) { vz3++; vz2--; } 
    // moons 2 - 4 
    // x 
    // x axis 
    if (px2 < px4) { vx2++; vx4--; } 
    if (px4 < px2) { vx4++; vx2--; } 
    // y axis 
    if (py2 < py4) { vy2++; vy4--; } 
    if (py4 < py2) { vy4++; vy2--; } 
    // z axis 
    if (pz2 < pz4) { vz2++; vz4--; } 
    if (pz4 < pz2) { vz4++; vz2--; } 
    // moons 3 - 4 
    // x 
    // x axis 
    if (px3 < px4) { vx3++; vx4--; } 
    if (px4 < px3) { vx4++; vx3--; } 
    // y axis 
    if (py3 < py4) { vy3++; vy4--; } 
    if (py4 < py3) { vy4++; vy3--; } 
    // z axis 
    if (pz3 < pz4) { vz3++; vz4--; } 
    if (pz4 < pz3) { vz4++; vz3--; } 
    px1 = px1 + vx1;
    py1 = py1 + vy1;
    pz1 = pz1 + vz1;
    px2 = px2 + vx2;
    py2 = py2 + vy2;
    pz2 = pz2 + vz2;
    px3 = px3 + vx3;
    py3 = py3 + vy3;
    pz3 = pz3 + vz3;
    px4 = px4 + vx4;
    py4 = py4 + vy4;
    pz4 = pz4 + vz4;
    count++; // steps we have computed 
    if (px1 != -1LL) { continue; }
    if (py1 != 0LL) { continue; }
    if (pz1 != 2LL) { continue; }
    if (px2 != 2LL) { continue; }
    if (py2 != -10LL) { continue; }
    if (pz2 != -7LL) { continue; }
    if (px3 != 4LL) { continue; }
    if (py3 != -8LL) { continue; }
    if (pz3 != 8LL) { continue; }
    if (px4 != 3LL) { continue; }
    if (py4 != 5LL) { continue; }
    if (pz4 != -1LL) { continue; }
    break;

  }
  count++;
  printf("met itself %" PRId64 "\n ", count);
  return 0;
}
