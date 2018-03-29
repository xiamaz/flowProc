#include "get_dir.h"

void free_fcs_file(fcs_file f) {
	if (f.fullpath) {
		free(f.fullpath);
	}
	if (f.group) {
		free(f.group);
	}
	if (f.material) {
		free(f.material);
	}
	if (f.label) {
		free(f.label);
	}
	if (f.dataset) {
		free(f.dataset);
	}
}

void destroy_list(fcs_list *l) {
	fcs_list *n;
	while (l) {
		n = l;
		l = l->next;
		free_fcs_file(n->val);
		free(n);
	}
}

void print_fcs_file(fcs_file f) {
	printf("%s: %s; Set %d\n", f.group, f.label, f.tube_set);
}

void print_list(fcs_list *fl) {
	while (fl) {
		print_fcs_file(fl->val);
		fl = fl->next;
	}
}

void add_link(fcs_list **l, char *group, char *filepath) {
	(*l)->next = calloc(1, sizeof(**l));
	*l = (*l)->next;
	(*l)->val.group = malloc(strlen(group));
	strcpy((*l)->val.group, group);
	(*l)->val.fullpath = malloc(strlen(filepath));
	strcpy((*l)->val.fullpath, filepath);
}

size_t match_to_buffer(char *buffer, char *str, regmatch_t rm) {
	size_t mlen = rm.rm_eo - rm.rm_so;
	memcpy(buffer, str + rm.rm_so, mlen);
	buffer[mlen] = '\0';
	return mlen;
}

void fill_matches(fcs_list *l, char *filename, regmatch_t *rm) {
	char *buffer = calloc(strlen(filename), 1);
	size_t len;
	len = match_to_buffer(buffer, filename, rm[1]);
	l->val.label = calloc(len+1, 1);
	memcpy(l->val.label, buffer, len);
	// printf("label: %s\n", l->val.label);

	len = match_to_buffer(buffer, filename, rm[2]);
	l->val.material = calloc(len+1, 1);
	memcpy(l->val.material, buffer, len);
	// printf("material: %s\n", l->val.material);

	match_to_buffer(buffer, filename, rm[3]);
	l->val.tube_set = buffer[0] - '0';
	// printf("tube: %d\n", l->val.tube_set);

	free(buffer);
}

size_t get_dir(char *pdir, char *dataset, fcs_list **fl) {
	// char *pdir;
	char *gname, *gpath, *filename;
	DIR *dir, *gdir;
	int reti;

	fcs_list *fl_head, *fl_end;
	struct dirent *files, *gfiles;

	regex_t regex;
	regmatch_t regmatches[4];

	fl_head = fl_end = calloc(1, sizeof(*fl_head));
	// crawl directory for files
	dir = opendir(pdir);

	// create regex
	regcomp(
		&regex,
		"([[:digit:]]+-[[:digit:]]+)-([[:alnum:]]+) CLL 9F 0([[:digit:]]).*\\.LMD",
		// "^\\([[:digit:]]\\{2\\}-[[:digit:]]+\\)-\\([[:alnum:]]+\\) CLL 9F 0\\([[:digit:]]\\) .*\\.LMD$",
		REG_EXTENDED
	);

	size_t i=0;
	if (dir) {
		while ((files = readdir(dir))) {
			gname = files->d_name;
			// ignore hidden and dotfiles
			if (gname[0] != '.') {
				// printf("normal %s\n", files->d_name);
				gpath = calloc(strlen(pdir) + strlen(gname) + 2, 1);
				sprintf(gpath, "%s/%s", pdir, gname);
				gdir = opendir(gpath);
				if (gdir) {
					while ((gfiles = readdir(gdir))) {
						filename = gfiles->d_name;
						// continue if not lmd file
						reti = regexec(&regex, filename, 4, regmatches, 0);
						if (!reti) {
							// new link in list
							fl_end->next = calloc(1, sizeof(*fl_end));
							fl_end = fl_end->next;
							fill_matches(fl_end, filename, regmatches);

							fl_end->val.group = calloc(strlen(gname)+1, 1);
							memcpy(fl_end->val.group, gname, strlen(gname));

							fl_end->val.fullpath = calloc(
									strlen(filename)+strlen(gpath)+2, 1);
							sprintf(fl_end->val.fullpath, "%s/%s", gpath, filename);

							fl_end->val.dataset = calloc(strlen(dataset)+1, 1);
							memcpy(fl_end->val.dataset, dataset, strlen(dataset));
							i += 1;
						}
					}
					closedir(gdir);
				}
				free(gpath);
			}
		}
		closedir(dir);
	}
	// for (fcs_list *l = fl_head; l != NULL; l = l->next, i++){
	// 	printf("List: %s %s\n", l->val.group, l->val.label);
	// }
	// printf("Number: %d\n", i);
	regfree(&regex);
	*fl = fl_head;

	return i;
}

// int main() {
// 	fcs_list *fl;
// 	size_t num = get_dir("../../mll_data/2017_11_multiclass", "test", &fl);
// 	print_list(fl);
// 	printf("Number of %ld\n", num);
// 
// 	destroy_list(fl);
// }

SEXP c_get_dir(SEXP dir_path, SEXP dataset) {
	char *dpath, *dset;
	size_t len = 1;
	PROTECT(dir_path = AS_CHARACTER(dir_path));
	PROTECT(dataset = AS_CHARACTER(dataset));
	dpath = R_alloc(strlen(CHAR(STRING_ELT(dir_path, 0))) + 1, sizeof(*dpath));
	strcpy(dpath, CHAR(STRING_ELT(dir_path, 0)));
	dset = R_alloc(strlen(CHAR(STRING_ELT(dataset, 0))) + 1, sizeof(*dset));
	strcpy(dset, CHAR(STRING_ELT(dataset, 0)));
	UNPROTECT(2);

	fcs_list *fl, *fcur;
	len = get_dir(dpath, dset, &fl);

	SEXP list, list_names, sublist;
	SEXP obj;

	char *names[5] = {"filepath", "group", "label", "material", "tube_set"};
	PROTECT(list_names = allocVector(STRSXP, 5));
	for (int i = 0; i < 5; i++) {
		SET_STRING_ELT(list_names, i, mkChar(names[i]));
	}

	PROTECT(list = allocVector(VECSXP, len));
	fcur = fl;
	// print_list(fcur);
	int i = 0;
	for (; fcur; fcur = fcur->next){
		if (!fcur->val.label) {
			continue;
		}
		PROTECT(obj = R_do_new_object(R_do_MAKE_CLASS("FlowEntry")));

		PROTECT(sublist = allocVector(STRSXP, 1));
		SET_STRING_ELT(sublist, 0, mkChar(fcur->val.fullpath));
		R_do_slot_assign(obj, mkChar("filepath"), sublist);
		UNPROTECT(1);

		PROTECT(sublist = allocVector(STRSXP, 1));
		SET_STRING_ELT(sublist, 0, mkChar(fcur->val.group));
		R_do_slot_assign(obj, mkChar("group"), sublist);
		UNPROTECT(1);

		PROTECT(sublist = allocVector(STRSXP, 1));
		SET_STRING_ELT(sublist, 0, mkChar(fcur->val.label));
		R_do_slot_assign(obj, mkChar("label"), sublist);
		UNPROTECT(1);

		PROTECT(sublist = allocVector(STRSXP, 1));
		SET_STRING_ELT(sublist, 0, mkChar(fcur->val.dataset));
		R_do_slot_assign(obj, mkChar("dataset"), sublist);
		UNPROTECT(1);

		PROTECT(sublist = allocVector(STRSXP, 1));
		SET_STRING_ELT(sublist, 0, mkChar(fcur->val.material));
		R_do_slot_assign(obj, mkChar("material"), sublist);
		UNPROTECT(1);

		PROTECT(sublist = NEW_NUMERIC(1));
		NUMERIC_POINTER(sublist)[0] = fcur->val.tube_set;
		R_do_slot_assign(obj, mkChar("tube_set"), sublist);
		UNPROTECT(1);

		SET_VECTOR_ELT(list, i, obj);
		i += 1;
	}
	// cleanup
	destroy_list(fl);
	UNPROTECT(2 + len);
	return list;
}


static const R_CallMethodDef callMethods[] = {
	{"c_get_dir", (DL_FUNC) &c_get_dir, 2},
	{NULL, NULL, 0}
};

void R_init_get_dir(DllInfo *info) {
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
