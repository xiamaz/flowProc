#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List huge(int n) {
	S4 test;
	List output(n);
	for (int i = 0; i < n; i++) {
		test = S4("testobj");
		test.slot("t") = "Teststring";
		// output[i] = List::create("Yello", "Yorld");
		output[i] = test;
	}
	return output;
}
