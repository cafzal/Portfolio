package hw6;


/**
 * 
 * @author Cameron Afzal
 * Recursively returns the number of ancestors who originated in a particular country.
 *
 */
public class Lineage {
	private String country;
	private Lineage parent;
	
	public Lineage (String c, Lineage p) {
		country = c; 
		parent = p;
	}
	
	public int numCountry(String c) {
		int num = 0;
		if (parent == null) {
			if (country == c) {
				num += 1;
			}
			return num;
		}
		
		if (country == c) {
			num += 1;
		}
		return num += parent.numCountry(c);
	}
}
