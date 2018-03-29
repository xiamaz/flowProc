#ifndef GET_DIR
#define GET_DIR
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <dirent.h>
#include <regex.h>
#include <stdio.h>
#include <string.h>
#endif
typedef struct {
	char *fullpath;
	char *group;
	char *material;
	char *label;
	char *dataset;
	unsigned char tube_set;
} fcs_file;

typedef struct fcs_list {
	struct fcs_list *next;
	fcs_file val;
} fcs_list;

// linked list operations
void free_fcs_file(fcs_file);
void destroy_list(fcs_list *);
void print_fcs_file(fcs_file);
void print_list(fcs_list *);
void add_link(fcs_list **l, char *group, char *filepath);

// put regex match into a previously allocated scratch buffer
size_t match_to_buffer(char *buffer, char *str, regmatch_t rm);

// put regex matches into the fcs_file struct, allocate struct
// internal fields as necessary
void fill_matches(fcs_list *l, char *filename, regmatch_t *rm);

// c internal get directory operation, crawl hierarchical directory
// put information into fcs_file list
size_t get_dir(char *pdir, char *dataset, fcs_list **fl);

// r exposed function to get_dir
SEXP c_get_dir(SEXP dir_path, SEXP dataset);

void R_init_get_dir(DllInfo *info);
