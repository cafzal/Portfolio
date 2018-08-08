package hw7;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;

import hw7.Graph2.Node;

public class GraphSearchAlgorithms {
	// Algorithm recursively traverses graph starting at root, exploring as far as possible in a path before backtracking.
	public void dfs(Node node) {
		System.out.print(node.val+" -> ");
		node.visited = true;
		for(Node n:node.adjacent) {
			if (!n.visited && n!=null) {
				dfs(n);
			}
		}
	}
	
	// Algorithm iteratively traverses graph starting at root level by level.
	public void bfs(Node node) {
		Queue<Node> queue = new LinkedList<Node>();
		HashSet<Node> visited = new HashSet<Node>();
		
		queue.add(node);
		visited.add(node);
		
		while(!queue.isEmpty()) {
			node = queue.poll();
			System.out.print(node.val + " -> ");
			for(Node n:node.adjacent) {
				if(!visited.contains(n)) {
					visited.add(n);
					queue.add(n);
				}
			}
		}
}
}
