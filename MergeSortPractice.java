package hw6;

import java.util.Arrays;

/**
 * 
 * Recursively subdivides array into subarrays then merges them back together.
 * Base case: array with 1 element.
 * Returns: new sorted array of all elements.
 *
 */
public class MergeSortPractice {
	
	public int[] sort(int[] arr) {
		int len = arr.length;
		int [] left;
		int [] right;
		
		if (arr.length <= 1) {
			return arr;
		}
		
		if (len%2 == 0) {
			left = new int[len/2];
			right = new int[len/2]; }
		else {
			left = new int[len/2];
			right = new int[(len/2)+1];
		}
		
		for (int i=0; i<len; i++) {
			if (i<(len/2)) {
				left[i] = arr[i];
			}
			else {
				right[i-(len/2)] = arr[i];
			}
		}
		
		left = sort(left);
		right = sort(right);
		
		return merge(left,right);
	}
	
	public int[] merge(int[] left, int[] right) {
		int[] merged = new int[left.length+right.length];
		int headL = 0;
		int headR = 0;

		for (int j = 0; j<merged.length; j++) {
			if (headL < left.length && headR < right.length) {
				if (left[headL] < right[headR]) {
					merged[j] = left[headL];
					headL++;
				}
				else {
					merged[j] = right[headR];
					headR++;
				}
			}
			else if (headL < left.length) {
				merged[j] = left[headL];
				headL++;
			}
			else {
				merged[j] = right[headR];
				headR++;
			}
		}
	return merged;
	}
}
