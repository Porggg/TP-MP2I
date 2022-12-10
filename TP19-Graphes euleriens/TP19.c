#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

#include "stack.h"
#include "graph.h"

int *read_data(int *nb_vertex, int *nb_edges){
  scanf("%d %d", nb_vertex, nb_edges);
  int *data = malloc(2 * *nb_edges * sizeof(int));
  for (int i = 0; i < *nb_edges; i++) {
    int x, y;
    scanf("\n%d %d", &x, &y);
    data[2*i] = x;
    data[2*i+1] = y;
  }
  return data;
}

stack *euler_tour(graph g){
  int p = g.nb_edges;
  stack *euler = stack_new(2*p);
  stack *current = stack_new(2*p);
  stack_push(current, 0);
  while (!stack_is_empty(current)) {
    int v = stack_peek(current);
    if (has_available_edge(g, v)) {
      edge e = get_edge(g, v);
      stack_push(current, e.to);
      delete_edge(g, e);
    } else {
      stack_pop(current);
      stack_push(euler, v);
    }
  }
  stack_free(current);
  return euler;
}

int main(void){
  int nb_vertex;
  int nb_edges;
  int *data = read_data(&nb_vertex, &nb_edges);
  graph g = build_graph(data, nb_vertex, nb_edges);
  stack *tour = euler_tour(g);
  while (!stack_is_empty(tour)) {
    int v = stack_pop(tour);
    printf("%d ", v);
  }
  printf("\n");
  stack_free(tour);
  graph_free(g);
  return 0;
}