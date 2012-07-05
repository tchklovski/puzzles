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

inline uint bits_to_bytes(uint bits) {
    return (bits + 7) / 8;
}
inline uint same_row(uint a, uint b) {
    return a / w == b / w;
}
inline int get_bit(byte* vector, uint offset) {
    return (vector[offset / 8] & 1 << (offset % 8)) != 0;
}
inline void set_bit(byte* vector, uint offset) {
    vector[offset / 8] |= 1 << (offset % 8);
}
inline void clear_bit(byte* vector, uint offset) {
    vector[offset / 8] &= ~(1 << (offset % 8));
}
inline uint is_free(uint offset) {
    return !get_bit(not_usable, offset) && !get_bit(visited, offset);
}

void is_vertex_cut_dfs(uint curr) {
    set_bit(reachable, curr);

    if (curr >= w && !get_bit(reachable, curr - w))
        is_vertex_cut_dfs(curr - w);
    if (same_row(curr, curr + 1) && curr + 1 < w * h && !get_bit(reachable, curr + 1))
        is_vertex_cut_dfs(curr + 1);
    if (curr + w < w * h && !get_bit(reachable, curr + w))
        is_vertex_cut_dfs(curr + w);
    if (curr >= 1 && same_row(curr, curr - 1) && !get_bit(reachable, curr - 1))
        is_vertex_cut_dfs(curr - 1);
}

uint is_vertex_cut() {
    uint dfs_start = -1;
    for (uint i = 0; i < w * h; i++)
        if (is_free(i))
            dfs_start = i;
    if (dfs_start == -1)
        return 0;

    memcpy(reachable, not_usable, bits_to_bytes(w * h));
    for (uint i = 0; i < w * h; i++)
        if (get_bit(visited, i))
            set_bit(reachable, i);

    is_vertex_cut_dfs(dfs_start);

    for (uint i = 0; i < w * h; i++)
        if (!get_bit(reachable, i))
            return 1;
    return 0;
}

uint min_degree_lt_2(uint curr) {
    uint degree;
    for (uint i = 0; i < w * h; i++) {
        if (is_free(i) && i != end) {
            degree = 0;
            if (i >= w && (is_free(i - w) || i - w == curr))
                degree += 1;
            if (i + 1 < w * h && same_row(i, i + 1)
                    && (is_free(i + 1) || i + 1 == curr))
                degree += 1;
            if (i + w < w * h && (is_free(i + w) || i + w == curr))
                degree += 1;
            if (i >= 1 && same_row(i, i - 1)
                    && (is_free(i - 1) || i - 1 == curr))
                degree += 1;
            if (degree < 2)
                return 1;
        }
    }
    return 0;
}

uint count_paths(uint curr, uint path_length) {
    if (path_length == max_path_length && curr == end) {
        return 1;
    } else if (is_vertex_cut() || min_degree_lt_2(curr)) {
        return 0;
    } else {
        int num_paths = 0;
        if (curr >= w && is_free(curr - w)) {
            set_bit(visited, curr - w);
            num_paths += count_paths(curr - w, path_length + 1);
            clear_bit(visited, curr - w);
        }
        if (same_row(curr, curr + 1) && curr + 1 < w * h
                && is_free(curr + 1)) {
            set_bit(visited, curr + 1);
            num_paths += count_paths(curr + 1, path_length + 1);
            clear_bit(visited, curr + 1);
        }
        if (curr + w < w * h && is_free(curr + w)) {
            set_bit(visited, curr + w);
            num_paths += count_paths(curr + w, path_length + 1);
            clear_bit(visited, curr + w);
        }
        if (curr >= 1 && same_row(curr, curr - 1)
                && is_free(curr - 1)) {
            set_bit(visited, curr - 1);
            num_paths += count_paths(curr - 1, path_length + 1);
            clear_bit(visited, curr - 1);
        }
        return num_paths;
    }
}

int main() {
    scanf("%d %d\n", &w, &h);

    not_usable = malloc(bits_to_bytes(w * h));
    max_path_length = 0;
    uint curr;
    for (uint i = 0; i < w * h; i++) {
        scanf("%d", &curr);

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

    reachable = malloc(bits_to_bytes(w * h));
    visited = calloc(bits_to_bytes(w * h), 1);
    set_bit(visited, start);

    uint num_paths = count_paths(start, 1);
    printf("%d\n", num_paths);

    free(not_usable);
    free(reachable);
    free(visited);

    return 0;
}
