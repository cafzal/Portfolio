package hw5;

import java.util.Arrays;
import java.util.HashSet;

public class NQueens {
// Check if current queen is safe
static boolean isSafe(int[][] board, int r, int c) {
	for (int i=0; i<=r; i++) {
		if (board[i][c]==1) {
			return false;
		}
	}
	for (int j=0; j<=c; j++) {
		if (board[r][j] == 1)
			return false;
	}
	return true;
}

//Solve N queens problem
static boolean solveNQueens(int n, int[][] board, int row, int col) {
	boolean success = false;
	if (n==0) {success = true;}
	for (int r= 0; r<board.length; r++) {
		for(int c=0; c<board.length; c++) {
			boolean safe = isSafe(board, r, c);
			if (safe==true) {
				board[r][c]=1;
				return solveNQueens(n-1, board, r, c);
			}
		}
	}
	return success;
}

//Create set to store all solutions 
static HashSet<int[][]> solveAllNQueens(int n, int[][] board, int row, int col) {
	HashSet<int[][]> set = new HashSet<int[][]>();
	if (n==0) {set.add(board);}
	for (int r= 0; r<board.length; r++) {
		for(int c=0; c<board.length; c++) {
			boolean safe = isSafe(board, r, c);
			if (safe==true) {
				board[r][c]=1;
				return solveAllNQueens(n-1, board, r, c);
			}
		}
	}
	return set;
}
}
