
#include "lst_structs.h"
#include "lst_stree.h"
#include "lst_string.h"
#include "lst_algorithms.h"
#include <Rdefines.h>

static LST_String * getStringRef(SEXP sstring);
static LST_StringSet * getStringSetRef(SEXP sset);
static LST_STree *getSuffixTreeRef(SEXP stree);
static void * getRef(SEXP sset, SEXP sym);

void finalizeString(SEXP s) {
    LST_String *string = getStringRef(s);
    if(string) {
		lst_string_free(string);
    }
    R_ClearExternalPtr(s);
}

void finalizeStringSet(SEXP s) {
    LST_StringSet *set = getStringSetRef(s);
    if(set) {
		free((void *) set); 
    }
    R_ClearExternalPtr(s);
}

void finalizeSuffixTree(SEXP s) {
    LST_STree *tree = getSuffixTreeRef(s);
    if(tree) {
		lst_stree_clear(tree);
		lst_stree_free(tree);
    }
    R_ClearExternalPtr(s);
}


SEXP makeMatchRef(int nmatches) {
    SEXP myint;	
	int *p_myint;
	PROTECT(myint = NEW_INTEGER(1)); // Allocating storage space
	p_myint = INTEGER_POINTER(myint); // ponit to SEXP object
	p_myint[0] = AS_INTEGER(nmatches);
	UNPROTECT(1);
	return myint;
}
 

SEXP makeStringRef(LST_String *string) {
    SEXP ans;
    PROTECT(ans = R_MakeExternalPtr(string, Rf_install("sstring"), R_NilValue));
    R_RegisterCFinalizer(ans, finalizeString);
    UNPROTECT(1);
	return(ans);
}

SEXP makeStringSetRef(LST_StringSet *set) {
    SEXP ans;
    PROTECT(ans = R_MakeExternalPtr(set, Rf_install("sset"), R_NilValue));
    R_RegisterCFinalizer(ans, finalizeStringSet);
    UNPROTECT(1);
return(ans);
}

SEXP makeSuffixTreeRef(LST_STree *stree) {
    SEXP ans;
    PROTECT(ans = R_MakeExternalPtr(stree, Rf_install("stree"), R_NilValue));
    R_RegisterCFinalizer(ans, finalizeSuffixTree);
    UNPROTECT(1);
return(ans);
}

SEXP R_newstree(SEXP sset) {
    LST_STree *tree;
    LST_StringSet *set;
    set = getStringSetRef(sset);
    tree = lst_stree_new(set);
return(makeSuffixTreeRef(tree));
}

SEXP R_newString(SEXP els) {
    const char *str;
	LST_String *string;
	str = CHAR(STRING_ELT(els, 0));
    string = lst_string_new(str, 1, strlen(str));
return(makeStringRef(string));
}

SEXP R_newsset(SEXP els) {
    LST_StringSet *set;
    int i, n;
    const char *str;
    set = lst_stringset_new();
    n = GET_LENGTH(els);
    for(i = 0; i < n; i++) {
		str = CHAR(STRING_ELT(els, i));
		lst_stringset_add(set, lst_string_new(str, 1, strlen(str)));
    }
return(makeStringSetRef(set));
}


LST_String * getStringRef(SEXP sstring) {
	return((LST_String *) getRef(sstring, Rf_install("sstring")));
}

LST_StringSet * getStringSetRef(SEXP sset) {
return((LST_StringSet *) getRef(sset, Rf_install("sset")));
}

LST_STree * getSuffixTreeRef(SEXP sstree) {
return((LST_STree *) getRef(sstree, Rf_install("stree")));
}

void * getRef(SEXP sset, SEXP sym) {
    LST_StringSet *set;
    if(TYPEOF(sset) != EXTPTRSXP) {
		PROBLEM "a %s reference must be an external pointer object in R",
	    CHAR(PRINTNAME(sym))
        ERROR;
    }
    if(R_ExternalPtrTag(sset) != sym) {
		PROBLEM "the %s reference has the wrong internal type/tag",
	    CHAR(PRINTNAME(sym))
        ERROR;
    }
    set = R_ExternalPtrAddr(sset);
return(set);
}

SEXP R_streeLongestSubstring(SEXP stree, SEXP lens, SEXP repeated) {
	LST_StringSet *resultSet;
	LST_STree *tree;
	LST_StringSet *(*f)(LST_STree *, u_int , u_int);
	
	f = LOGICAL(repeated)[0] ? lst_alg_longest_repeated_substring : lst_alg_longest_common_substring;
	tree = getSuffixTreeRef(stree);
	
	resultSet = f(tree, INTEGER(lens)[0], INTEGER(lens)[1]);
	
return(makeStringSetRef(resultSet));
}

static void countStringSet(LST_String *str, void *ctr) {
	int *c = (int *) ctr;
	(*c)++;
}

int lst_stringset_length(LST_StringSet *set) {
    int ctr = 0;
    lst_stringset_foreach(set, countStringSet, &ctr);  
return(ctr);
}

SEXP R_stringSetAdd(SEXP sset, SEXP values) {
    LST_StringSet *set;
    int i, n;
    set = getStringSetRef(sset);
    n = GET_LENGTH(values);
	
    for(i = 0; i < n; i++) {
		const char *str = CHAR(STRING_ELT(values, i));
		lst_stringset_add(set, lst_string_new(str, 1, strlen(str)));
    }	
return(ScalarInteger(lst_stringset_length(set)));
}

SEXP R_stringSetRemove(SEXP sset, SEXP values) {
    LST_StringSet *set;
    int i, n;
    set = getStringSetRef(sset);
    n = GET_LENGTH(values);
	
    for(i = 0; i < n; i++) {
		const char *str = CHAR(STRING_ELT(values, i));
		lst_stringset_remove(set, lst_string_new(str, 1, strlen(str)));
    }	
	return(ScalarInteger(lst_stringset_length(set)));
}

SEXP R_streeAddRemove(SEXP stree, SEXP sset, SEXP add) {
    LST_StringSet *set;
	LST_STree *tree;
	
    set = getStringSetRef(sset);
	tree = getSuffixTreeRef(stree);
	
	LST_String *string;
	for (string = set->members.lh_first; string; string = string->set.le_next) {
		LOGICAL(add)[0] ? lst_stree_add_string(tree, string) : lst_stree_remove_string(tree, string);
	}

return(ScalarInteger(tree->num_strings));
}

typedef SEXP (*R_LST_NativeIterator)(LST_String *str, SEXP data);
typedef struct {
	int i; /* Current location */
    SEXP els; /* the answer list into which we put the individual elements */
    SEXP call; /* for calling the R function */
   	R_LST_NativeIterator op; /* If we are dealing with a native routine rather than an R function. */
  	Rboolean  isRFunction; /* Indicates whether we have an R function or native routine. */
} ForEachData ;

void forEachCallback(LST_String *str, void *cbdata) {
	ForEachData *data = (ForEachData *) cbdata;
	SEXP val;
	if(data->isRFunction) {
 		const char *tmp = lst_string_print(str);
 		SETCAR(CDR(data->call), mkString(tmp ? tmp : ""));
 		val = Rf_eval(data->call, R_GlobalEnv);
 	} else {
 		val = data->op(str, data->call);
 	} 
 	if(data->i >= GET_LENGTH(data->els)) {
 		return;
 	}
 	SET_VECTOR_ELT(data->els, data->i, val);
data->i++;
}

SEXP R_lapplyStringSet(SEXP sset, SEXP fun, SEXP args) {
	SEXP tmp;
	int i, n, nargs;
	LST_StringSet *set;
	ForEachData data;
	set = getStringSetRef(sset);
	n = lst_stringset_length(set);
	data.i = 0;
	PROTECT(data.els = NEW_LIST(n));
	if(TYPEOF(fun) == CLOSXP) {
 		data.isRFunction = TRUE;
 		nargs = GET_LENGTH(args);
 		PROTECT(data.call = allocVector(LANGSXP, 2 + nargs));
 		SETCAR(data.call, fun);
 		SETCAR(CDR(data.call), R_NilValue); /* Fill in empty value. */
       	if(nargs > 0) {
 			tmp = CDR(CDR(data.call));
 			for(i = 0; i < nargs; i++) {
 				SETCAR(tmp, VECTOR_ELT(args, i));
 				tmp = CDR(tmp);
 			}
 		}
 	} else if(TYPEOF(fun) == EXTPTRSXP) {
 		data.isRFunction = FALSE;
 		data.op = (R_LST_NativeIterator) R_ExternalPtrAddr(fun);
 		if(GET_LENGTH(args))
 			args = VECTOR_ELT(args, 0);
 		data.call = args;
 	} else {
 		PROBLEM "Unrecognized operator in the lapply. Need an R function or a routine"
 		ERROR;
 	}
 	lst_stringset_foreach(set, forEachCallback, &data);
 	UNPROTECT(data.isRFunction ? 2 : 1); 
return(data.els);
}

typedef struct {
	int i; /* Current location */
	SEXP els; /* for calling the R function */
} bfsSearchData ;

void bfsSearchCallback(LST_Node *node, void *bfsdata) {
	bfsSearchData *data = (bfsSearchData *) bfsdata;
 	SET_VECTOR_ELT(data->els, data->i, node->index);
data->i++;	
}

SEXP R_bfsSearch(SEXP stree, SEXP sset) {
	
	LST_StringSet *set;
	LST_STree *tree;

    set = getStringSetRef(sset);
	tree = getSuffixTreeRef(stree);
	
	int n = lst_stringset_length(set);

	
	LST_String *string;
	string = set->members.lh_first;
	//for (string = set->members.lh_first; string; string = string->set.le_next) {
	
		bfsSearchData data;
		PROTECT(data.els = NEW_LIST(n));
	
	
	
	lst_alg_bfs(tree,  (LST_NodeVisitCB) bfsSearchCallback, &data);
	//}
	
	return(data.els);
}



SEXP R_sSearch(SEXP stree, SEXP sset, SEXP add) {
		
	LST_StringSet *set;
	LST_STree *tree;
	
	bfsSearchData data;
	//data.tree = tree;
	
    set = getStringSetRef(sset);
	tree = getSuffixTreeRef(stree);
	
	printf("%i\n", tree->num_strings);
	
	
	LST_String *string;
	
	for (string = set->members.lh_first; string; string = string->set.le_next) {
		
		int index = -1;
		LST_StringHash *hash;
		LST_StringHashItem *hi;
		LST_StringHashItem *ritem;
		
		hash = &tree->string_hash[string->id % LST_STRING_HASH_SIZE];
		//printf("%s\n", lst_string_print(string));
	
		for (hi = hash->lh_first; hi; hi = hi->items.le_next) {
			if (hi->string->id == string->id) {
				printf("found\n");
				index  = hi->index;
				ritem = hi;
				break;
				
			}
		}
		if(index>0) {
			printf("%i\n", ritem->index);
			printf("%s\n", lst_string_print(ritem->string));
		}

	}

		
	//lst_alg_bfs(tree,  (LST_NodeVisitCB) bfsSearchCallback, &data);
	

return(ScalarInteger(tree->num_strings));
}


SEXP R_streePrint(SEXP stree) {
	LST_STree *tree;
	tree = getSuffixTreeRef(stree);
	lst_debug_print_tree(tree);
return(ScalarInteger(tree->num_strings));
}

typedef struct lst_path_end
{
	LST_Node    *node;
	
	LST_Edge    *edge;
	u_int        offset;
	
} LST_PathEnd;

SEXP R_followStringSlow(SEXP stree, SEXP sset) {
	LST_STree *tree;
	LST_StringSet *set;
	set = getStringSetRef(sset);
	tree = getSuffixTreeRef(stree);

	int mnmatches=0;
	LST_PathEnd *end;
	LST_String *string;
	for (string = set->members.lh_first; string; string = string->set.le_next) {
		u_int num_matches = lst_stree_follow_string_slow(tree, tree->root_node, string, end);
		//u_int nstr = lst_string_get_length(string);
		
		if(num_matches>mnmatches) {
			mnmatches=num_matches;
		}
		//printf("%i\t%i\t%i\n", i, num_matches, nstr);
		//printf("%i\n", end->offset);
		
		//printf("%i\n", end->node->bus_visited);
		//printf("%i\n", lst_string_print(end->edge->range.string));
		
	/*	LST_Edge *edge;
		for (edge = end->node->kids.lh_first; edge; edge = edge->siblings.le_next) {
			printf("%s\n", lst_string_print(edge->range.string));
		}
	*/	
	//	i++;
	}
return(ScalarInteger(mnmatches));
	//return(ScalarInteger(tree->num_strings));
}

