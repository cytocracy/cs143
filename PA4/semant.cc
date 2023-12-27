

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}


//return 
template<typename Key, typename Value>
Value *ClassTable::lookup(std::unordered_map<Symbol, SymbolTable<Key, Value>> &symtab, Symbol class_name, Symbol key){
    auto end = symtab.end();
    while (*class_name != *No_class) {
        auto it = symtab.find(class_name);
        if (it == end){
            semant_error() << "Class " << class_name << " not found\n";
            return nullptr;
        }

        auto &st = it->second;
        auto value = st.lookup(key);
        if (value != nullptr) {
            return value;
        }

        auto cls = get_class(class_name);
        class_name = cls->get_parent();
    }

    return nullptr;
}


ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    install_basic_classes();

    unordered_set<Symbol> invalid_parents{Bool, Str};

    for (auto cls: *classes){
        auto class_name = cls->get_name();
        auto parents = cls->get_parent();

        if (class_name == SELF_TYPE || classes_.find(class_name) != classes_.end()) {
            semant_error(cls) << class_name << "redeclared" << endl;
            return;
        }

        auto it = invalid_parents.find(parent);
        if (it != invalid_parents.end()) {
            semant_errro(cls) << "Class " << cls->get_name() << " cannot inherit from " << *it << "\n";
            return;
        }

        classes_[class_name] = cls;
    }

    //make sure parent exists
    for (auto cls: *classes){
        auto parent = cls->get_parent();
        auto class_name = cls->get_name();
        if (parent != No_class && clsses_.find(parent) == classes_.end()) {
            semant_error(cls) << "Class " << class_name << " inherits from non existent class " << parent << endl;
            return;
        }
    }

    bool v[classes_.size()];

    //check if hiercharcy is a circle
    if (accumulate(
        v,
        transform(
            begin(classes_),
            end(classes_),
            v,
            [this] (const decltype(*begin(classes_)) &elem) -> bool {
                return has_cycle(elem.second);
            }
        ),
        false,
        logical_or<bool>()
    ))
    {
        return;
    }

    //need main class
    if (classes_.find(Main) == classes_.end()) {
        semant_error() << "Class Main is not defined. \n";
        //return;
    }

    for (auto cls: classes_) {
        attrs[cls.second->get_name()].enterscope();
        methods[cls.second->get_name()].enterscope();
    }

    vector<Symbol> predefined_classes{Object, IO, Int, Bool, Str};
    for (auto cls_name: predefined_classes) {
        auto cls = get_class(cls_name);
        set_current_class(cls);
        for (auto feature: *cls->get_features()) {
            feature->add(this);
            if (errors()) {
                return;
            }
        }
    }

    for (auto cls : *classes)
    {
        set_current_class(cls);
        for (auto feature : *cls->get_features())
        {
            feature->add(this);
            if (errors())
            {
                return;
            }
        }
    }

    for (auto cls : *classes)
    {
        set_current_class(cls);
        for (auto feature : *cls->get_features())
        {
            feature->type_check(this);
            if (errors())
            {
                return;
            }
        }
    }
}


//confirm derived is a subclass of ancestor
bool ClassTable::leq(Symbol derived, Symbol ancestor) {
    ancestor = get_class(acnestor)->get_name();
    derived = get_class(derived)->get_name();
    while (*derived != *No-class) {
        if (*derived == *ancestor) {
            return true;
        }
        auto cls = get_class(derived);
        if (cls == nullptr){
            return false;
        }
        derived = cls->get_parent();
    }

    return false;
}


Class_ ClassTable::get_class(Symbol name, bool report) {
    if (*name == *SELF_TYPE)
        return get_class();
    auto it = classes_.find(name);
    if (it == classes_.end()) {
        if (report) {
            semant_error() << "Class " << name << " was not found\n";
        }
        return nullptr;
    }

    return it->second;
}

//find cycle starting at any class
bool ClassTable::has_cycle(Class_ cls) {
    auto name = cls->get_name();
    std::unordered_set<decltype(name)> s{ name };
    auto e = s.end();

    for (auto parent = cls->get_parent(); parent != No_class; parent = classes_[parent]->get_parent()) {
        if (s.find(parent) != e) {
            semant_error(classes_[parent]) << "Found hierachy cycle";
            return true;
        }

        s.insert(parent);
    }

    return false;
}

Symbol ClassTable::find_common_ancestor(Symbol a, Symbol b) {
    auto ca = get_clas(a);
    if (ca == nullptr) {
        return nullptr;
    }

    auto cb = get_class(b);
    if (cb == nullptr) {
        return nullptr;
    }

    //get all ancestors of class a
    std::unordered_set<Symbol> s{ca->get_name()};
    for (auto class_name = ca->get_parent(); *class_name != *No_class; class_name = ca->get_parent()) {
        s.insert(class_name);
        ca = get_class(class_name);
    }

    //get ancestors of class B, return first one that is in common with a
    for (auto class_name = cb->get_name(); *class_name != *No_class; class_name = cb->get_parent()) {
        cb = get_class(class_name);
        if (s.find(class_name) != s.end()) {
            return class_name;
        }
    }

    //no common acnestor
    return nullptr;
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object, 
            No_class,
            append_Features(
                    append_Features(
                            single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                            single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                    single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
            filename);
        classes_[object] = Object_class;

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
            Object,
            append_Features(
                    append_Features(
                            append_Features(
                                    single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                SELF_TYPE, no_expr())),
                                    single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                SELF_TYPE, no_expr()))),
                            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                    single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
            filename);
        classes_[IO] = IO_class;

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
        classes_[Int] = Int_class;

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    classes_[Bool] = Bool_class;

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
    classes_[Str] = Str_class;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    //all type checking is done in constructor
    ClassTable *classtable = new ClassTable(classes);

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

//-------------------------------------------------------------------------------

void method_class::add(ClassTable *p) {
    auto cls = p->get_class();
    auto &symtab = p->methods.find(cls->get_name())->second;

    if (symtab.probe(name)) {
        p->semant_error(clc) << "Method redefinition: " << name << "\n";
        return;
    }

    symtab.addid(name, this);
}

void attr_class:add(ClassTable *p) {
    auto cls = p->get_class();

    if (*name == *self) {
        p->semant_error(cls) << "'self' cannot be the name of an attribute\n";
        return;
    }

    //check if attr is alr defined
    if (p->lookup(p->attrs, cls->get_name(), name) != nullptr) {
        p->semant_error(cls) << "Attribute redefinition: " << name << "\n";
        return;
    }

    auto &symtab = p->attrs.find(cls->get_name())->second;
    symtab.addid(name, type_decl);
}

//---------------------------------------------------------------------------------


void method_class::type_check(ClassTable *p) {
    //check return type is defined
    if (p->get_class(return_type, false) == nullptr) {
        p->semant_error(p->get_class()) << "Undefined return type " << return_type << " for method " << name << "\n";
        return;
    }

    auto &symtab = p->attrs.find(p->get_class()->get_name())->second;
    symtab.enterscope();

    //enters parameter in symtab
    for (auto formal: *formals) {
        auto fc = reinterpret_cast<formal_class*>(formal);
        if (*fc->name == *self) {
            p->semant_error(p->get_class()) << "parameter cannot be bnamed 'self'\n";
            return;
        }
        if (*fc->type_decl == *SELF_TYPE) {
            p->semant_error(p->get_class()) << "Parameter cannot be typed 'SELF_TYPE'\n";
            return;
        }
        if (symtab.probe(fc->name)) {
            p->semant_error(p->get_class()) << "Duplicated parameter name " << fc->name << " of method " << name << "\n";
            return;
        }
        if (p->get_class(fc->type_decl) == nullptr) {
            return;
        }

        symtab.addid(fc->name, fc->type_decl);
    }

    auto parent_method = p->lookup(p->methods, p->get_class()->get_parent(), name);
    if (parent_method != nullptr) {
        if (formals->len() != parent_method->formals->len()) {
            symtab.exitscope();
            p->semant_error(p->get_class()) << "Redefined method " << name << " has different number of paremeters\n";
            return;
        }
        auto e = end(*formals);
        mismatch(begin(*formals), e, begin(*parent_method->formals), [=](Formal a, Formal b) -> bool {
            auto f1 = reinterpret_cast<formal_class*>(a);
            auto f2 = reinterpret_cast<formal_class*>(b);
            auto ret = *f1->type_decl == *f2->type_decl;
            if (!ret) {
                p->semant_error(p->get_class()) << "In redefined method " << name << ", parameter type " << f1->type_decl << " is different from original type " << f2->type_decl << "\n";
            }
            return ret;
        });
    }

    //validate return type
    auto rettype = expr->type_check(p);
    if (rettype != nullptr) {
        if (*return_type == *SELF_TYPE && *return_type != *rettype) {
            p->semant_error(p->get_class()) << "Inferred return type " << rettype << " of method " << name << " does not conform to declared return type " << return_type << "\n";
        } else if (!p->leq(rettype, return_type)) {
            p->semant_error(p->get_class()) << "Inferred return type " << rettype << " of method " << name MM " does not conform to declared return type " << return_type << "\n";
        }
    }
    symtab.exitscope();
}

//next is attr_class::type_check


