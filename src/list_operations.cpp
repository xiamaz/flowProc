#include <algorithm>
#include <regex>
#include <sstream>
#include <dirent.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::plugins("cpp14")]]

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

//' @export
// [[Rcpp::export]]
SEXP cppGetDir (String dir_path, String dataset) {
	struct dirent *files;
	std::regex filename_regex("^(\\d+-\\d+)-(\\w+) CLL 9F (\\d+).*.LMD$");
	std::smatch regex_match;
	DIR *dir = opendir(dir_path.get_cstring());
	if (dir == nullptr) {
		Rprintf("Invalid directory %s\n", dir_path.get_cstring());
		return List::create();
	}
	// std::vector<S4> file_objs;
	std::vector<List> file_objs;
	int i = 0;
	// S4 test;
	List test;
	DIR *subdir;
	std::stringstream ss;
	while (bool(files = readdir(dir))) {
		auto group = std::string(files->d_name);
		if (group == "." || group == "..") {
			continue;
		}
		// clear the stringstream
		ss.str(std::string());
		ss << std::string(dir_path) << "/" << group;
		std::string subpath = ss.str();
		subdir = opendir(subpath.c_str());
		if (subdir == nullptr) {
			continue;
		}
		// Rprintf("Listing %s\n", subpath.c_str());
		struct dirent *fcs_file;
		while (bool(fcs_file = readdir(subdir))) {
			//Rprintf("%s\n", fcs_file->d_name);
			std::string filename(fcs_file->d_name);
			// match regular expressions to filename
			if (std::regex_match(filename, regex_match, filename_regex)) {
				if (regex_match.size() == 4) {
					ss.str(std::string());
					ss << subpath << "/" << filename;
					std::string filepath = ss.str();

					// test = S4("FlowEntry");
					// test.slot("filepath") = filepath;
					// test.slot("group") = group;
					// test.slot("label") = regex_match[1].str();
					// test.slot("material") = regex_match[2].str();
					// test.slot("tube_set") = std::stoi(regex_match[3].str());
					// test.slot("dataset") = dataset;
					test = List::create(
							Named("filepath") = filepath,
							Named("group") = group,
							Named("label") = regex_match[1].str(),
							Named("material") = regex_match[2].str(),
							Named("tube_set") = std::stoi(regex_match[3].str()),
							Named("dataset") = dataset);
					file_objs.push_back(test);
				}
			}
		}
		closedir(subdir);
	}
	closedir(dir);
	// List output = List(file_objs.size());
	// // Rprintf("End stuff\n");
	// for (int i = 0; i < file_objs.size(); i++) {
	// 	Rprintf("Sting %d of %d\n", i, file_objs.size());
	// 	output[i] = file_objs[i];
	// }
	return wrap(file_objs);
}
