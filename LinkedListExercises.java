package LinkedList;

/** An instance is a doubly linked list. */
public class LinkedListExercises<E>  {
    private Node first; // first node of linked list (null if size is 0)
    private Node last;  // last node of linked list (null if size is 0)
    private int size;   // Number of values in the linked list.

    /** Constructor: an empty linked list. */
    public LinkedListExercises() {
    }

    /** Return the number of values in this list.
     *  This function takes constant time. */
    public int size() {
        return size;
    }

    /** Return the first node of the list (null if the list is empty). */
    public Node first() {
        return first;
    }

    /** Return the last node of the list (null if the list is empty). */
    public Node last() {
        return last;
    }

    /** Return the value of node n of this list.
     * Precondition: n is a node of this list; it may not be null. */
    public E value(Node n) {
        assert n != null;
        return n.val;
    }

    /** Return a representation of this list: its values, with adjacent
     * ones separated by ", ", "[" at the beginning, and "]" at the end. <br>
     * Takes time proportional to the length of this list.<br>
     * E.g. for the list containing 4 7 8 in that order, the result it "[4, 7, 8]".
     * E.g. for the list containing two empty strings, the result is "[, ]" */
    public String toString() {
        String res= "[";
        Node n= first;
        // inv: res contains values of nodes before node n (all of them if n = null),
        //      with ", " after each (except for the last value)
        while (n != null) {
            res= res + n.val;
            n= n.next;
            if (n != null) {
                res= res + ", ";
            }
        }

        return res + "]";
    }

    /** Return a representation of this list: its values in reverse, with adjacent
     * ones separated by ", ", "[" at the beginning, and "]" at the end. <br>
     * Note that gnirtSot is the reverse of toString.
     * Takes time proportional to the length of this list.
     * E.g. for the list containing 4 7 8 in that order, the result is "[8, 7, 4]".
     * E.g. for the list containing two empty strings, the result is "[, ]". */
    public String gnirtSot() { // Note: 
    	String res = "[";
    	Node n = last;
    	while (n != null) {
    		res = res + n.val;
    		n = n.prev;
    		if (n != null) {
    			res = res + ", ";
    		}
    	}
        return res + "]";
    }
    

    /** Add value v in a new node at the front of the list.
     * This operation takes constant time. */
    public void prepend(E v) {
    	size += 1;
    	Node n = new Node(null, v, first);
    	if (first != null) {
            first.prev = n;
    	}
    	first = n;
    	if (n.next == null) {
    		last = n;
    	}
    }

    /** add value v in a new node at the end of the list.
     *  This operation takes constant time. */
    public void append(E v) {
        size += 1;
        Node n = new Node(last, v, null);
        if (last != null) {
        	last.next = n;
        }
        last = n;
        if (n.prev == null) {
        	first = n;
        }
    }


    /** Return node number k. 
     *  Precondition: 0 <= k < size of the list.
     *  If k is 0, return first node; if k = 1, return second node, ... */
    public Node getNode(int k) {
        assert 0 <= k;
        assert k < size;
    	if (k == 0) {
        	return first;
        }
    	else if (k == size-1) {
    		return last;
    	}
    	else if (k <= size/2) {
    		Node n = first;
    		int i = 0;
    		//Node a = n;
    		while (i < k) {
    			n = n.next;
    			i++;
    		}
    		return n;
    	}
    	else if (k >= size/2) {
    		Node n = last;
    		int i = 0;
    		while (i < k) {
    			n = n.prev;
    			i++;
    		}
    		return n;
    	}
        
    	else {
        return null;
    	}
    }
    
    /** Remove node n from this list.
     * This operation must take constant time.
     * Precondition: n must be a node of this list; it may not be null. */
    public void delete(Node n) {
    	assert n != null && first != null;
    	if (size > 1) {assert n.prev.next != null || n.next.prev != null;};
    	n.prev.next = n.next;
    	n.next.prev = n.prev;
    	size--;
    }

    /** Insert value v in a new node after node n.
     * This operation takes constant time.
     * Precondition: n must be a node of this list; it may not be null. */
    public void insertAfter(E v, Node n) {
    	assert n != null && first != null;
    	if (size > 1) {assert n.next != null || n.prev != null;}
    	size += 1;
    	Node z = new Node(n, v, n.next);
    	if (z.next != null) {z.next.prev = z;}
    	n.next = z;
    }

 

    /*********************/

    /** An instance is a node of this list. */
    public class Node {
        private Node prev; // Previous node on list (null if this is first node)
        private E val;     // The value of this element
        private Node next; // Next node on list. (null if this is last node)

        /** Constructor: an instance with previous node p (can be null),
         * value v, and next node n (can be null). */
        Node(Node p, E v, Node n) {
            prev= p;
            val= v;
            next= n;
        }

        /** Return the node previous to this one (null if this is the
         * first node of the list). */
        public Node prev() {
            return prev;
        }

        /** Return the value of this node. */
        public E value() {
            return val;
        }

        /** Return the next node in this list (null if this is the
         * last node of this list). */
        public Node next() {
            return next;
        }
    }

}
