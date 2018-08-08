package hw7;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;

import hw7.Graph2.Node;

public class GraphSearchAlgorithms {
	public void dfs(Node node) {
		System.out.print(node.val+" -> ");
		node.visited = true;
		for(Node n:node.adjacent) {
			if (!n.visited && n!=null) {
				dfs(n);
			}
		}
	}
	
	
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
