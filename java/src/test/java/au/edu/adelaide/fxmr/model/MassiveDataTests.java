package au.edu.adelaide.fxmr.model;

import org.junit.Test;
import static org.junit.Assert.*;

public class MassiveDataTests {

	double[][] means = { { 0.87, 0.75, 0.8, 0.92, 0.86, 0.83, 0.63, 0.55, 0.5600000000000001, 0.92, 0.77, 0.75, 0.55, 0.46, 0.44, 0.71,
			0.71, 0.61, 0.61, 0.8100000000000001, 0.8100000000000001, 0.8100000000000001, 0.79, 0.79, 0.79, 0.7, 0.7, 0.7, 0.84, 0.84, 0.84,
			0.76, 0.76, 0.76, 0.86, 0.86, 0.86, 0.84, 0.84, 0.84, 0.79, 0.79, 0.79, 0.94, 0.48, 0.92, 0.46, 0.89, 0.58, 0.75, 0.51, 0.89,
			0.94, 0.89, 0.99, 0.96, 0.96, 0.94, 0.94, 0.82, 0.8966, 0.7586000000000001, 0.9167, 0.7738, 0.9370000000000001, 0.904, 0.98,
			0.95, 0.96, 1, 0.97, 0.96, 0.73, 0.44, 0.46, 0.9300000000000001, 0.84, 0.84, 0.83, 0.83, 0.83, 0.79, 0.79, 0.79, 0.73, 0.73,
			0.73, 0.76, 0.76, 0.76, 0.71, 0.71, 0.71, 0.77, 0.77, 0.77, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89,
			0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.83, 0.83, 0.83, 0.83,
			0.83, 0.83, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.96, 0.96,
			0.96, 0.96, 0.96, 0.96, 0.97, 0.97, 0.97, 0.97, 0.97, 0.97, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.88, 0.88, 0.88, 0.88, 0.88,
			0.88, 0.86, 0.86, 0.86, 0.86, 0.86, 0.86, 1, 0.7, 0.95, 0.725, 1, 0.7901, 0.963, 0.7654, 0.963, 0.6543, 0.847, 0.771, 0.725,
			0.577, 0.542, 0.482, 0.79, 0.79, 0.79, 0.8100000000000001, 0.8100000000000001, 0.8100000000000001, 0.742, 0.742, 0.742, 0.913,
			0.913, 0.913, 0.955, 0.955, 0.955, 0.892, 0.892, 0.892 },
			{ 0.53, 0.4, 0.53, 0.73, 0.6800000000000001, 0.61, 0.64, 0.61, 0.52, 0.7, 0.59, 0.51, 0.58, 0.5, 0.49, 0.46, 0.1, 0.38, 0.08,
					0.09, 0.84, 0.18, 0.21, 0.83, 0.23, 0.25, 0.8, 0.24, 0.2, 0.8100000000000001, 0.19, 0.13, 0.74, 0.19, 0.15,
					0.8100000000000001, 0.15, 0.24, 0.7, 0.13, 0.13, 0.78, 0.19, 0.41, 0.41, 0.41, 0.4, 0.66, 0.6, 0.58, 0.53, 0.22, 0.25,
					0.24, 0.05, 0.07000000000000001, 0.07000000000000001, 0.04, 0.05, 0.41, 0.5948, 0.1552, 0.5595, 0.0357, 0.342, 0.067,
					0.05, 0.06, 0.07000000000000001, 0.06, 0.06, 0.09, 0.06, 0.06, 0.09, 0.06, 0.06, 0.09, 0.71, 0.22, 0.23,
					0.6800000000000001, 0.13, 0.18, 0.77, 0.23, 0.41, 0.6800000000000001, 0.1, 0.33, 0.61, 0.32, 0.35, 0.66, 0.26, 0.31,
					0.64, 0.24, 0.31, 0.51, 0.22, 0.14, 0.8, 0.53, 0.13, 0.22, 0.17, 0.18, 0.65, 0.36, 0.09, 0.18, 0.07000000000000001,
					0.14, 0.8, 0.53, 0.13, 0.22, 0.17, 0.18, 0.65, 0.36, 0.09, 0.18, 0.07000000000000001, 0.14, 0.78, 0.82, 0.18, 0.12,
					0.03, 0.1, 0.82, 0.78, 0.02, 0.02, 0.17, 0.05, 0.78, 0.82, 0.18, 0.12, 0.03, 0.1, 0.82, 0.78, 0.02, 0.02, 0.17, 0.05,
					0.63, 0.65, 0.27, 0.27, 0.1, 0.05, 0.7, 0.6800000000000001, 0.07000000000000001, 0.07000000000000001,
					0.07000000000000001, 0.05, 0.63, 0.65, 0.27, 0.27, 0.1, 0.05, 0.7, 0.6800000000000001, 0.07000000000000001,
					0.07000000000000001, 0.07000000000000001, 0.05, 0.53, 0.04, 0.04, 0.47, 0.04, 0.03, 0.525, 0.8, 0.725, 0.85, 0.3951,
					0.4444, 0.6667, 0.8395, 0.4198, 0.5926, 0.478, 0.511, 0.368, 0.468, 0.528, 0.34, 0.629, 0.008, 0.024, 0.65,
					0.07000000000000001, 0.05, 0.645, 0.024, 0.024, 0.6, 0.15, 0.15, 0.6790000000000001, 0.223, 0.125, 0.708, 0.208,
					0.092 },
			{ 0.88, 0.8100000000000001, 0.83, 0.96, 0.83, 0.74, 0.59, 0.62, 0.55, 0.89, 0.8100000000000001, 0.76, 0.53, 0.48, 0.46, 0.86,
					0.86, 0.78, 0.78, 0.89, 0.89, 0.89, 0.78, 0.78, 0.78, 0.86, 0.86, 0.86, 0.86, 0.86, 0.86, 0.74, 0.74, 0.74, 0.83, 0.83,
					0.83, 0.86, 0.86, 0.86, 0.85, 0.85, 0.85, 0.95, 0.7, 0.75, 0.63, 0.91, 0.63, 0.67, 0.63, 0.9300000000000001, 0.9, 0.85,
					0.98, 0.92, 0.9300000000000001, 0.95, 0.95, 0.94, 0.8704, 0.6481, 0.9022, 0.587, 0.779, 0.691, 0.98, 0.94, 0.94, 1,
					0.9300000000000001, 0.92, 0.86, 0.62, 0.66, 0.96, 0.85, 0.86, 0.82, 0.82, 0.82, 0.85, 0.85, 0.85, 0.88, 0.88, 0.88,
					0.83, 0.83, 0.83, 0.74, 0.74, 0.74, 0.73, 0.73, 0.73, 0.6800000000000001, 0.6800000000000001, 0.6800000000000001, 0.84,
					0.84, 0.84, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.74, 0.74, 0.74, 0.74, 0.74, 0.74,
					0.8100000000000001, 0.8100000000000001, 0.8100000000000001, 0.8100000000000001, 0.8100000000000001, 0.8100000000000001,
					1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98,
					0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 1, 1, 1, 1, 1, 1, 0.97, 0.97, 0.97, 0.97, 0.97, 0.97, 1, 1,
					1, 0.97, 0.97, 0.97, 1, 0.975, 0.625, 0.8, 1, 0.8929, 0.8333, 0.7381, 0.8333, 0.75, 0.779, 0.666, 0.621, 0.648, 0.519,
					0.439, 0.903, 0.903, 0.903, 0.888, 0.888, 0.888, 0.942, 0.942, 0.942, 0.88, 0.88, 0.88, 0.892, 0.892, 0.892, 0.967,
					0.967, 0.967 },
			{ 0.6, 0.45, 0.44, 0.74, 0.66, 0.54, 0.83, 0.76, 0.58, 0.7, 0.58, 0.48, 0.66, 0.59, 0.55, 0.72, 0.23, 0.61, 0.18, 0.31, 0.88,
					0.59, 0.15, 0.9300000000000001, 0.26, 0.3, 0.86, 0.51, 0.05, 0.9300000000000001, 0.21, 0.21, 0.9, 0.45, 0.14, 0.91,
					0.26, 0.3, 0.79, 0.43, 0.19, 0.79, 0.29, 0.84, 0.74, 0.67, 0.69, 0.84, 0.71, 0.7, 0.66, 0.55, 0.6, 0.53, 0.18, 0.23,
					0.3, 0.08, 0.14, 0.82, 0.7407, 0.037, 0.7283, 0.1196, 0.771, 0.096, 0.11, 0.17, 0.26, 0.11, 0.19, 0.28, 0.11, 0.19,
					0.28, 0.11, 0.19, 0.28, 0.83, 0.23, 0.28, 0.77, 0.28, 0.39, 0.8100000000000001, 0.22, 0.43, 0.82, 0.27, 0.53, 0.63,
					0.28, 0.59, 0.71, 0.28, 0.6800000000000001, 0.66, 0.26, 0.58, 0.76, 0.24, 0.69, 0.98, 0.72, 0.13, 0.36, 0.33, 0.42,
					0.98, 0.87, 0.15, 0.49, 0.5, 0.61, 0.98, 0.72, 0.13, 0.36, 0.33, 0.42, 0.98, 0.87, 0.15, 0.49, 0.5, 0.61, 1, 0.95, 0.13,
					0.13, 0.13, 0.1, 1, 0.95, 0.03, 0.07000000000000001, 0.17, 0.3, 1, 0.95, 0.13, 0.13, 0.13, 0.1, 1, 0.95, 0.03,
					0.07000000000000001, 0.17, 0.3, 0.98, 0.92, 0.41, 0.4, 0.47, 0.42, 1, 0.97, 0.38, 0.37, 0.5, 0.53, 0.98, 0.92, 0.41,
					0.4, 0.47, 0.42, 1, 0.97, 0.38, 0.37, 0.5, 0.53, 1, 0.12, 0.78, 0.98, 0.15, 0.84, 0.7, 0.875, 0.975, 0.95, 0.5476,
					0.4762, 0.9405, 0.9881, 0.5714, 0.7381, 0.548, 0.513, 0.363, 0.549, 0.603, 0.37, 0.9350000000000001, 0.161, 0.065, 0.85,
					0.1, 0.063, 0.875, 0.267, 0.083, 0.87, 0.261, 0.217, 0.9, 0.4, 0.067, 0.825, 0.367, 0.083 }
	};

	double[][] weightDiag = {
			{ 976, 976, 496, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 960, 960, 960, 960, 320, 320, 320, 320, 320, 320,
					320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 640, 640, 640, 640, 640, 640,
					640, 640, 640, 768, 768, 464, 464, 464, 544, 544, 7760, 464, 464, 368, 368, 960, 960, 464, 464, 464, 448, 448, 448, 448,
					448, 448, 448, 448, 448, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 464, 464, 464, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 320, 320, 320, 320, 432, 432, 432, 432, 432, 432, 9104, 9104,
					9104, 9104, 9104, 9104, 496, 496, 496, 400, 400, 400, 496, 496, 496, 320, 320, 320, 448, 448, 448, 480, 480, 480 },
			{ 976, 976, 496, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 960, 960, 960, 960, 320, 320, 320, 320, 320, 320,
					320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 640, 640, 640, 640, 640, 640,
					640, 640, 640, 768, 768, 464, 464, 464, 544, 544, 7760, 464, 464, 368, 368, 960, 960, 464, 464, 464, 448, 448, 448, 448,
					448, 448, 448, 448, 448, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 464, 464, 464, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 320, 320, 320, 320, 432, 432, 432, 432, 432, 432, 9104, 9104,
					9104, 9104, 9104, 9104, 496, 496, 496, 400, 400, 400, 496, 496, 496, 320, 320, 320, 448, 448, 448, 480, 480, 480 },
			{ 976, 976, 448, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 960, 960, 960, 960, 320, 320, 320, 320, 320, 320,
					320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 592, 592, 592, 592, 592, 592,
					592, 592, 640, 768, 768, 496, 496, 496, 464, 464, 7760, 432, 432, 336, 336, 960, 960, 448, 448, 448, 448, 448, 448, 448,
					448, 448, 448, 448, 448, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 464, 464, 464, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 320, 320, 320, 320, 448, 448, 448, 448, 448, 448, 7984, 7984,
					7984, 7984, 7984, 7984, 496, 496, 496, 320, 320, 320, 480, 480, 480, 368, 368, 368, 480, 480, 480, 480, 480, 480 },
			{ 976, 976, 448, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 960, 960, 960, 960, 320, 320, 320, 320, 320, 320,
					320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 320, 592, 592, 592, 592, 592, 592,
					592, 592, 640, 768, 768, 496, 496, 496, 464, 464, 7760, 432, 432, 336, 336, 960, 960, 448, 448, 448, 448, 448, 448, 448,
					448, 448, 448, 448, 448, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 464, 464, 464, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480,
					480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 480, 320, 320, 320, 320, 448, 448, 448, 448, 448, 448, 7984, 7984,
					7984, 7984, 7984, 7984, 496, 496, 496, 320, 320, 320, 480, 480, 480, 368, 368, 368, 480, 480, 480, 480, 480, 480 }
	};

	double regCov = 0.625;
	int ns = 61;

	@Test
	public void keepTestAnnotationImportedNotTest() {
	}

	// TOO MASSIVE!
	//@Test
	public void fitTest() {
		CMRxFitsProblemMaker maker = new CMRxFitsProblemMaker();
		int nVar = 4;

		maker.setModel(new double[] { 1, 0, 1, 0 });
		//maker.setModel(new double[][] { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } });

		int n = weightDiag[0].length;

		double[][] cov = new double[n][n];
		for (int i = 0; i < n; i++)
			cov[i][i] = regCov;

		for (int i = 0; i < nVar; i++) {
			maker.addMeanArray(means[i]);

			double[] d = weightDiag[i];
			double[][] w = new double[n][n];
			for (int j = 0; j < n; j++)
				w[j][j] = d[j];

			maker.addWeightArray(w);
			maker.addCov(cov);
		}

		maker.setN(new int[] { ns, ns, ns, ns });

		CMRxFits sol = new CMRxFits(1, maker.getProblem(), -1, -1, false, false, 0, 0, true);

		assertEquals(17880.4683592237, sol.getDataFit(), 1e-10);
	}
}