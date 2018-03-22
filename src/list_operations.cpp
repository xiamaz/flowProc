#include <algorithm>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

//' Group list of S4 flow entries by slot and optionally limit to tube
//'
//' Disable tube limiting by passing an empty vector such as vector().
//'
//' @param input List of flow entries.
//' @param group_on String for slot to group on.
//' @param tube_nums Integer vector for tube numbers to be used.
//' @export
// [[Rcpp::export]]
List cGroupBy (List input, String group_on, IntegerVector tube_nums) {
	std::map<String, std::list<S4>> groups;
	for (S4 obj : input) {
		String s = obj.slot(group_on);
		if (tube_nums.size() > 0) {
			int tube_num = obj.slot("tube_set");
			if (std::find(tube_nums.begin(), tube_nums.end(), tube_num)
					== tube_nums.end()) {
				continue;
			}
		}
		auto search = groups.find(s);
		if (search == groups.end()){
			groups[s] = {obj};
		} else {
			groups[s].push_back(obj);
		}
	}
	// erase from map based on filter requirement
	for (auto iter = groups.cbegin(); iter != groups.cend();){
		if ((tube_nums.size() > 0)
				&& (tube_nums.size() != iter->second.size())) {
			groups.erase(iter++);
		} else {
			++iter;
		}
	}

	List output(groups.size());
	CharacterVector names(groups.size());
	size_t i = 0;
	for (auto iter = groups.begin(); iter != groups.end(); ++iter, ++i) {
		output[i] = wrap(iter->second);
		names[i] = iter->first;
		// Rprintf("%s\n", iter->first.get_cstring());
		// for (S4 obj : groups[iter->first]){
		// 	int label = obj.slot("tube_set");
		// 	Rprintf("%d\n", label);
		// }
	}
	output.attr("names") = names;
	return output;
}
