// shamelessly adapted from https://github.com/ankurdave/datacenter-cooling
// with revised edge handling and a cutting tweak by Tim Chklovski

/* on my laptop, this tweaked version is 20%+ faster

MY TWEAK:

time ./cooling < test
301716

real	0m18.882s
user	0m18.733s
sys	0m0.120s

ORIGINAL:
time ./datacenter-cooling < test
301716

real	0m23.874s
user	0m23.713s
sys	0m0.048s

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned int uint;
typedef unsigned char byte;

// One bit per room: 0 if it is usable ("ours"), 1 if it is not usable
// ("not ours")
byte* not_usable;

// One bit per room: 1 if the current path visits it, 0 otherwise
byte* visited;

// One bit per room: 1 if it is reachable from the end of the
// current path, 0 otherwise
byte* reachable;

uint w, h, start, end, max_path_length;

int nbrs[4]; // neignbors -- finalized once we know w
int dnbrs[8]; // neighbors including the diagonal

inline uint bits_to_bytes(uint bits) {
    return (bits + 7) >> 3;
}
inline uint same_row(uint a, uint b) {
  return a / w == b / w;
}
inline int get_bit(byte* vector, uint offset) {
    return (vector[offset >> 3] & 1 << (offset & 7)) != 0;
}
inline void set_bit(byte* vector, uint offset) {
    vector[offset >> 3] |= 1 << (offset & 7);
}
inline void clear_bit(byte* vector, uint offset) {
    vector[offset >> 3] &= ~(1 << (offset & 7));
}
inline uint is_free(uint offset) {
    return !get_bit(not_usable, offset) && !get_bit(visited, offset);
}

void show(byte* vector) {
  for (uint j = 0; j < h+2; j++) {
    for (uint i = 0; i < w+1; i++) {
      printf ("%d ",get_bit(not_usable,j*(w+1)+i));
     }
    printf ("|%d\n", j);
  }
}

void is_vertex_cut_dfs(uint curr) {
    set_bit(reachable, curr);

    if (!get_bit(reachable, curr - w))
      is_vertex_cut_dfs(curr - w);
    if (!get_bit(reachable, curr + 1))
        is_vertex_cut_dfs(curr + 1);
    if (!get_bit(reachable, curr + w))
        is_vertex_cut_dfs(curr + w);
    if (!get_bit(reachable, curr - 1))
        is_vertex_cut_dfs(curr - 1);
}

uint is_vertex_cut() {
    uint dfs_start = -1;
    for (uint i = w; i < w * (h - 1); i++)
        if (is_free(i))
            dfs_start = i;
    if (dfs_start == -1)
        return 0;

    memcpy(reachable, not_usable, bits_to_bytes(w * h));
    for (uint i = w; i < (w - 1) * h; i++)
      if (get_bit(visited, i))
        set_bit(reachable, i);

    is_vertex_cut_dfs(dfs_start);

    for (uint i = w; i < w * (h - 1); i++)
      if (!get_bit(reachable, i))
        return 1;
    return 0;
}



uint min_degree_lt_2(uint curr) {
    uint degree;
    int pos;
    // good to do a full sweep once at start, but not every time
    for (uint i = w; i < w * (h - 1); i++) {
      // check one min degree
      if (is_free(i) && i != end) {
        degree = 0;
        if (is_free(i - w) || i - w == curr)
            degree += 1;
        if (is_free(i - 1) || i - 1 == curr)
          degree += 1;
        if (is_free(i + 1) || i + 1 == curr)
          degree += 1;
        if (is_free(i + w) || i + w == curr)
          degree += 1;
        if (degree < 2)
          return 1;
      }
    }
    return 0;
}

uint count_paths(uint curr, uint path_length) {
  /*
vertex-cut checs seem to cost 2.2 secs if not_ours is near current, chase it down with a dfs marking -- that you can roll back -- so also update a rollback;
if rollback present, apply it before returning
   */

    if (path_length == max_path_length && curr == end) {
        return 1;
    } else if (min_degree_lt_2(curr) || is_vertex_cut()) {
        return 0;
    } else {
        int num_paths = 0;
        if (is_free(curr - w)) {
            set_bit(visited, curr - w);
            num_paths += count_paths(curr - w, path_length + 1);
            clear_bit(visited, curr - w);
        }
        if (is_free(curr + 1)) {
            set_bit(visited, curr + 1);
            num_paths += count_paths(curr + 1, path_length + 1);
            clear_bit(visited, curr + 1);
        }
        if (is_free(curr + w)) {
            set_bit(visited, curr + w);
            num_paths += count_paths(curr + w, path_length + 1);
            clear_bit(visited, curr + w);
        }
        if (is_free(curr - 1)) {
            set_bit(visited, curr - 1);
            num_paths += count_paths(curr - 1, path_length + 1);
            clear_bit(visited, curr - 1);
        }
        return num_paths;
    }
}

int main() {
  int scanf_ref = scanf("%d %d\n", &w, &h);
  w = w+1; // we put an extra row of not_ours to make a vertical "edge"
  h = h+2; // we add a row before and after to make top and bottom edges

  nbrs[0] = -w;
  nbrs[3] =  w;

  dnbrs[0] = -1 - w;
  dnbrs[1] =    - w;
  dnbrs[2] =  1 - w;
  dnbrs[5] = -1 + w;
  dnbrs[6] =      w;
  dnbrs[7] =  1 + w;

  not_usable = malloc(bits_to_bytes(w*h));
  max_path_length = 0;
  uint curr;
  for (uint i = 0; i < w*h; i++) {
    if((i < w) || i >= w*h || ((i+1) % w == 0)){
      curr = 1;
    } else {
      int scanf_ret = scanf("%d", &curr);
    }

    if (curr == 1) {
      set_bit(not_usable, i);
    } else {
      clear_bit(not_usable, i);
      max_path_length++;
    }

    if (curr == 2)
      start = i;

    if (curr == 3)
      end = i;
  }

  reachable = malloc(bits_to_bytes((w+1) * (h+2)));
  visited = calloc(bits_to_bytes((w+1) * (h+2)), 1);
  set_bit(visited, start);

  uint num_paths = count_paths(start, 1);
  printf("%d\n", num_paths);

  free(not_usable);
  free(reachable);
  free(visited);
  return 0;
}
