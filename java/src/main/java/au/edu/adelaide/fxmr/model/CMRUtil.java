package au.edu.adelaide.fxmr.model;

import gnu.trove.set.hash.TIntHashSet;

public class CMRUtil {
	public static final long[] POW_3 = { 1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323,
			4782969, 14348907, 43046721, 129140163, 387420489, 1162261467, 3486784401l, 10460353203l, 31381059609l,
			94143178827l, 282429536481l, 847288609443l, 2541865828329l, 7625597484987l, 22876792454961l,
			68630377364883l, 205891132094649l, 617673396283947l, 1853020188851841l, 5559060566555523l,
			16677181699666569l, 50031545098999707l, 150094635296999121l, 450283905890997363l, 1350851717672992089l,
			4052555153018976267l };
	public static final long[] SUM_REVERSE_POW_3 = { 0, 1, 4, 13, 40, 121, 364, 1093, 3280, 9841, 29524, 88573, 265720,
			797161, 2391484, 7174453, 21523360, 64570081, 193710244, 581130733, 1743392200, 5230176601l, 15690529804l,
			47071589413l, 141214768240l, 423644304721l, 1270932914164l, 3812798742493l, 11438396227480l,
			34315188682441l, 102945566047324l, 308836698141973l, 926510094425920l, 2779530283277761l, 8338590849833284l,
			25015772549499853l, 75047317648499560l, 225141952945498681l, 675425858836496044l, 2026277576509488133l,
			6078832729528464400l };

	public static final long[][] REVERSE_POW_3 = { {}, { 1 }, { 3l, 1 }, { 9l, 3l, 1 }, { 27l, 9l, 3l, 1 },
			{ 81l, 27l, 9l, 3l, 1 }, { 243l, 81l, 27l, 9l, 3l, 1 }, { 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 }, { 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l,
					3l, 1 },
			{ 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l,
					27l, 9l, 3l, 1 },
			{ 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l,
					729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l,
					2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l,
					19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l,
					177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l,
					531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l,
					4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l,
					1 },
			{ 94143178827l, 31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l,
					14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l,
					9l, 3l, 1 },
			{ 282429536481l, 94143178827l, 31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l,
					43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l,
					243l, 81l, 27l, 9l, 3l, 1 },
			{ 847288609443l, 282429536481l, 94143178827l, 31381059609l, 10460353203l, 3486784401l, 1162261467l,
					387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l,
					6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 2541865828329l, 847288609443l, 282429536481l, 94143178827l, 31381059609l, 10460353203l, 3486784401l,
					1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l,
					59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 7625597484987l, 2541865828329l, 847288609443l, 282429536481l, 94143178827l, 31381059609l, 10460353203l,
					3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l,
					177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 22876792454961l, 7625597484987l, 2541865828329l, 847288609443l, 282429536481l, 94143178827l, 31381059609l,
					10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l,
					1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 68630377364883l, 22876792454961l, 7625597484987l, 2541865828329l, 847288609443l, 282429536481l,
					94143178827l, 31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l,
					43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l,
					243l, 81l, 27l, 9l, 3l, 1 },
			{ 205891132094649l, 68630377364883l, 22876792454961l, 7625597484987l, 2541865828329l, 847288609443l,
					282429536481l, 94143178827l, 31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l,
					129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l,
					2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 617673396283947l, 205891132094649l, 68630377364883l, 22876792454961l, 7625597484987l, 2541865828329l,
					847288609443l, 282429536481l, 94143178827l, 31381059609l, 10460353203l, 3486784401l, 1162261467l,
					387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l,
					6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 1853020188851841l, 617673396283947l, 205891132094649l, 68630377364883l, 22876792454961l, 7625597484987l,
					2541865828329l, 847288609443l, 282429536481l, 94143178827l, 31381059609l, 10460353203l, 3486784401l,
					1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l,
					59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 5559060566555523l, 1853020188851841l, 617673396283947l, 205891132094649l, 68630377364883l,
					22876792454961l, 7625597484987l, 2541865828329l, 847288609443l, 282429536481l, 94143178827l,
					31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l,
					4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l,
					1 },
			{ 16677181699666569l, 5559060566555523l, 1853020188851841l, 617673396283947l, 205891132094649l,
					68630377364883l, 22876792454961l, 7625597484987l, 2541865828329l, 847288609443l, 282429536481l,
					94143178827l, 31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l,
					43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l,
					243l, 81l, 27l, 9l, 3l, 1 },
			{ 50031545098999707l, 16677181699666569l, 5559060566555523l, 1853020188851841l, 617673396283947l,
					205891132094649l, 68630377364883l, 22876792454961l, 7625597484987l, 2541865828329l, 847288609443l,
					282429536481l, 94143178827l, 31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l,
					129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l,
					2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 150094635296999121l, 50031545098999707l, 16677181699666569l, 5559060566555523l, 1853020188851841l,
					617673396283947l, 205891132094649l, 68630377364883l, 22876792454961l, 7625597484987l,
					2541865828329l, 847288609443l, 282429536481l, 94143178827l, 31381059609l, 10460353203l, 3486784401l,
					1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l, 1594323l, 531441l, 177147l,
					59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 450283905890997363l, 150094635296999121l, 50031545098999707l, 16677181699666569l, 5559060566555523l,
					1853020188851841l, 617673396283947l, 205891132094649l, 68630377364883l, 22876792454961l,
					7625597484987l, 2541865828329l, 847288609443l, 282429536481l, 94143178827l, 31381059609l,
					10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l, 4782969l,
					1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l, 1 },
			{ 1350851717672992089l, 450283905890997363l, 150094635296999121l, 50031545098999707l, 16677181699666569l,
					5559060566555523l, 1853020188851841l, 617673396283947l, 205891132094649l, 68630377364883l,
					22876792454961l, 7625597484987l, 2541865828329l, 847288609443l, 282429536481l, 94143178827l,
					31381059609l, 10460353203l, 3486784401l, 1162261467l, 387420489l, 129140163l, 43046721l, 14348907l,
					4782969l, 1594323l, 531441l, 177147l, 59049l, 19683l, 6561l, 2187l, 729l, 243l, 81l, 27l, 9l, 3l,
					1 }, };

	public static void main(String[] g) {

		for (int n = 0; n < 40; n++) {
			long[] powers = new long[n];
			long sumP = 0;
			for (int j = 0; j < n; j++) {
				powers[j] = POW_3[n - j - 1];
				sumP += powers[j];
			}
			// System.out.print("{");
			// for (int i = 0; i < n; i++) {
			// System.out.print(powers[i]);
			// if (i != n - 1)
			// System.out.print("l,");
			// }
			// System.out.println("},");

			System.out.println(sumP + ",");

		}

		//
		//
		// long sum = 0;
		// for (int i = 0; i < 40; i++) {
		// sum += POW_3[i];
		// System.out.println(sum + ",");
		// }
	}

	public static TIntHashSet calcInfeasibleZoneNumbers(int nvar) {
		TIntHashSet infeas = new TIntHashSet();
		int nz = (int) ((POW_3[nvar] - 1) / 2);

		for (int i = 1; i <= nz; i++)
			if (zDecodeContainsNeg1(i, nvar))
				infeas.add(i);

		return infeas;
	}

	/**
	 * Returns sign vector of length n from vector of zone numbers z;
	 * 
	 * @param z
	 * @param n
	 * @return
	 */
	private static boolean zDecodeContainsNeg1(int z, int n) {
		long r = z;
		for (int j = 1; j <= n; j++)
			r += POW_3[n - j];
		int i = 1;
		int u = (int) (r / Math.pow(3, n - i)) - 1;
		if (u < 0)
			return true;
		for (i = 2; i <= n; i++) {
			r -= (1 + u) * POW_3[n - i + 1];
			u = (int) (r / POW_3[n - i]) - 1;
			if (u < 0)
				return true;
		}
		return false;
	}
}
