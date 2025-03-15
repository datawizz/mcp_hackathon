:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(dcg/basics)).

% Dynamic predicates for the double category system
:- dynamic double_category/1.
:- dynamic d_object/2.
:- dynamic d_morphism/6.  % Morphism now includes both string label and Prolog term
:- dynamic d_path/3.
:- dynamic rdf_root_dir/1.

% Default root directory
rdf_root_dir('./double_cat_db').

% Set the root directory for RDF files
set_rdf_root_dir(Dir) :-
    retractall(rdf_root_dir(_)),
    assert(rdf_root_dir(Dir)),
    format('Root directory set to: ~w~n', [Dir]).

% Get full file path from relative path
get_full_path(RelPath, FullPath) :-
    rdf_root_dir(RootDir),
    directory_file_path(RootDir, RelPath, FullPath).

% Get full TTL file path
get_ttl_path(RelPath, TTLPath) :-
    atom_concat(RelPath, '.ttl', TTLFile),
    get_full_path(TTLFile, TTLPath).

%% DCG for parsing predicate paths %%

% A path is a sequence of morphisms (predicates) separated by dots
path_expression(Path) -->
    object_id(Source),
    ['.'],
    path_sequence(Source, Target, Morphisms),
    { Path = d_path(Source, Target, Morphisms) }.

% A path sequence is a morphism followed by another path sequence or end
path_sequence(Source, Target, [Morphism|Rest]) -->
    morphism_id(Morphism),
    { d_morphism(Morphism, Source, NextObj, _, _, _) },
    (
        ['.'],
        path_sequence(NextObj, Target, Rest)
    ;
        { Target = NextObj, Rest = [] }
    ).

% Parse an object identifier
object_id(Object) -->
    identifier(Id),
    { d_object(Object, Id) }.

% Parse a morphism (predicate) identifier
morphism_id(Morphism) -->
    identifier(Id),
    { d_morphism(Morphism, _, _, _, Id, _) }.

% Parse an identifier (atom)
identifier(Id) -->
    alpha(First),
    alphanum_star(Rest),
    { atom_codes(Id, [First|Rest]) }.

% Parse a single alphabetic character
alpha(C) -->
    [C],
    { code_type(C, alpha) }.

% Parse zero or more alphanumeric characters
alphanum_star([C|Cs]) -->
    [C],
    { code_type(C, alnum) },
    alphanum_star(Cs).
alphanum_star([]) --> [].

%% Double Category Theory Predicate Management %%

% Define a new double category
define_double_category(Name) :-
    assert(double_category(Name)),
    format('Defined double category: ~w~n', [Name]).

% Define a new object in a double category
define_d_object(Category, ObjectId, Label) :-
    atom_concat(Category, ':', Prefix),
    atom_concat(Prefix, ObjectId, FullId),
    assert(d_object(FullId, Label)),
    format('Defined object: ~w with label ~w in category ~w~n', [FullId, Label, Category]).

% Define a new morphism (predicate) between objects with both string and Prolog term
define_d_morphism(Category, MorphismId, SourceObj, TargetObj, StringLabel, PrologTerm) :-
    atom_concat(Category, ':', Prefix),
    atom_concat(Prefix, MorphismId, FullId),
    assert(d_morphism(FullId, SourceObj, TargetObj, StringLabel, MorphismId, PrologTerm)),
    format('Defined morphism: ~w from ~w to ~w with label ~w and Prolog term~n',
           [FullId, SourceObj, TargetObj, StringLabel]).

% Parse a path expression string into a path term
parse_d_path(String, Path) :-
    atom_codes(String, Codes),
    phrase(path_expression(Path), Codes).

% Compose a path from a sequence of morphisms
compose_d_path(Source, Target, Morphisms) :-
    compose_d_path_recursive(Source, Target, Morphisms).

% Base case: empty path, source must equal target
compose_d_path_recursive(Obj, Obj, []).
% Recursive case: follow one morphism, then continue
compose_d_path_recursive(Source, Target, [Morphism|Rest]) :-
    d_morphism(Morphism, Source, NextObj, _, _, _),
    compose_d_path_recursive(NextObj, Target, Rest).

% Get the file path associated with a predicate path
d_path_to_file_path(d_path(Source, Target, Morphisms), FilePath) :-
    d_object(Source, SourceLabel),
    d_object(Target, TargetLabel),
    d_morphisms_to_path_segment(Morphisms, PathSegment),
    atomic_list_concat([SourceLabel, PathSegment, TargetLabel], '/', FilePath).

% Convert a list of morphisms to a path segment
d_morphisms_to_path_segment([], '').
d_morphisms_to_path_segment([Morphism], Label) :-
    d_morphism(Morphism, _, _, StringLabel, _, _),
    atom_concat(StringLabel, '', Label).
d_morphisms_to_path_segment([Morphism|Rest], PathSegment) :-
    d_morphism(Morphism, _, _, StringLabel, _, _),
    d_morphisms_to_path_segment(Rest, RestSegment),
    atomic_list_concat([StringLabel, RestSegment], '/', PathSegment).

%% Evaluating Prolog Terms Against RDF Database %%

% Evaluate a single morphism (predicate) against the RDF database
evaluate_morphism(Morphism, Subject, Object, RDFGraph) :-
    d_morphism(Morphism, _, _, _, _, PrologTerm),
    % Create a variable binding for Subject and Object
    PrologCall =.. [PrologTerm, Subject, Object, RDFGraph],
    % Call the Prolog term as a predicate
    call(PrologCall).

% Evaluate a complete path against the RDF database
evaluate_path(d_path(Source, Target, Morphisms), Subject, Object, RDFGraph) :-
    evaluate_path_recursively(Morphisms, Subject, Object, RDFGraph).

% Base case for empty path
evaluate_path_recursively([], Subject, Subject, _).
% Case for single morphism
evaluate_path_recursively([Morphism], Subject, Object, RDFGraph) :-
    evaluate_morphism(Morphism, Subject, Object, RDFGraph).
% Case for multiple morphisms
evaluate_path_recursively([Morphism|Rest], Subject, FinalObject, RDFGraph) :-
    evaluate_morphism(Morphism, Subject, IntermediateObject, RDFGraph),
    evaluate_path_recursively(Rest, IntermediateObject, FinalObject, RDFGraph).

%% Path Generation Based on RDF Content %%

% Generate all valid paths between Source and Target objects
generate_paths(Source, Target, MaxLength, Paths) :-
    findall(Path,
            (
                between(1, MaxLength, Length),
                generate_path_of_length(Source, Target, Length, Path)
            ),
            Paths).

% Generate a path of specific length
generate_path_of_length(Source, Target, 1, d_path(Source, Target, [Morphism])) :-
    d_morphism(Morphism, Source, Target, _, _, _).
generate_path_of_length(Source, Target, Length, d_path(Source, Target, [Morphism|RestMorphisms])) :-
    Length > 1,
    d_morphism(Morphism, Source, Intermediate, _, _, _),
    NewLength is Length - 1,
    generate_path_of_length(Intermediate, Target, NewLength, d_path(Intermediate, Target, RestMorphisms)).

% Find all paths that can be validated based on RDF content
find_valid_rdf_paths(Source, Target, MaxLength, RDFGraph, ValidPaths) :-
    generate_paths(Source, Target, MaxLength, CandidatePaths),
    findall(Path,
            (
                member(Path, CandidatePaths),
                Path = d_path(_, _, Morphisms),
                check_path_validity(Source, Target, Morphisms, RDFGraph)
            ),
            ValidPaths).

% Check if a path is valid in the RDF graph
check_path_validity(Source, Target, Morphisms, RDFGraph) :-
    % Find subjects of type Source in RDF
    rdf_subject_of_type(Source, Subjects, RDFGraph),
    % For at least one subject, check if the path leads to a valid target
    member(Subject, Subjects),
    follow_path(Subject, Morphisms, Target, RDFGraph).

% Find all subjects of a specific type in RDF
rdf_subject_of_type(Type, Subjects, RDFGraph) :-
    findall(Subject,
            rdf(Subject, rdf:type, Type, RDFGraph),
            Subjects).

% Follow a path from Subject through Morphisms to check if it reaches a Target
follow_path(Subject, [], _, _).
follow_path(Subject, [Morphism|Rest], Target, RDFGraph) :-
    d_morphism(Morphism, _, _, _, _, PrologTerm),
    % Call the Prolog term to get the next object
    PrologCall =.. [PrologTerm, Subject, NextObject, RDFGraph],
    call(PrologCall),
    % Continue with the rest of the path
    follow_path(NextObject, Rest, Target, RDFGraph).

%% RDF Integration for Double Category %%

% Initialize the RDF database for a graph
init_rdf_db(RelPath) :-
    rdf_retractall(_, _, _, RelPath),
    % Register prefixes
    rdf_register_prefix(cat, 'http://example.org/category-theory#'),
    rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'),
    rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#'),
    rdf_register_prefix(ex, 'http://example.org/resource#').

% Load RDF data from a TTL file
load_ttl(RelPath) :-
    get_ttl_path(RelPath, TTLPath),
    (   exists_file(TTLPath)
    ->  catch(
            rdf_load_turtle(TTLPath, [graph(RelPath)]),
            Error,
            (write('Error loading file: '), write(Error), nl, fail)
        ),
        format('Successfully loaded RDF data from ~w~n', [TTLPath])
    ;   format('File not found: ~w~n', [TTLPath]),
        fail
    ).

% Save RDF data to a TTL file
save_ttl(RelPath) :-
    get_ttl_path(RelPath, TTLPath),
    % Ensure directory exists
    file_directory_name(TTLPath, Dir),
    (   exists_directory(Dir)
    ->  true
    ;   make_directory_path(Dir),
        format('Created directory: ~w~n', [Dir])
    ),
    catch(
        rdf_save_turtle(TTLPath, [graph(RelPath)]),
        Error,
        (write('Error saving file: '), write(Error), nl, fail)
    ),
    format('Successfully saved RDF data to ~w~n', [TTLPath]).

%% Custom Prolog Predicates for RDF Patterns %%

% Direct RDF relation (direct triple)
rdf_direct_relation(Subject, Object, Predicate, RDFGraph) :-
    rdf(Subject, Predicate, Object, RDFGraph).

% Inverse relation
rdf_inverse_relation(Subject, Object, Predicate, RDFGraph) :-
    rdf(Object, Predicate, Subject, RDFGraph).

% Type-based relation (get all objects of a type that are related to the subject)
rdf_type_relation(Subject, Object, Predicate, RDFGraph) :-
    rdf(Subject, Predicate, Object, RDFGraph),
    rdf(Object, rdf:type, _, RDFGraph).

% Property path relation (follow a property path)
rdf_property_path(Subject, Object, [Predicate], RDFGraph) :-
    rdf(Subject, Predicate, Object, RDFGraph).
rdf_property_path(Subject, Object, [Predicate|Rest], RDFGraph) :-
    rdf(Subject, Predicate, Intermediate, RDFGraph),
    rdf_property_path(Intermediate, Object, Rest, RDFGraph).

% Transitive closure relation (e.g., parent+ = ancestor)
rdf_transitive_relation(Subject, Object, Predicate, RDFGraph) :-
    rdf_tc(Subject, Predicate, Object, RDFGraph, []).

% Helper for transitive closure with cycle detection
rdf_tc(Subject, Predicate, Object, RDFGraph, Visited) :-
    rdf(Subject, Predicate, Object, RDFGraph).
rdf_tc(Subject, Predicate, Object, RDFGraph, Visited) :-
    rdf(Subject, Predicate, Intermediate, RDFGraph),
    \+ member(Intermediate, Visited),
    rdf_tc(Intermediate, Predicate, Object, RDFGraph, [Intermediate|Visited]).

% Property value match (object with a specific property value)
rdf_property_value(Subject, Object, prop_value(Prop, Value), RDFGraph) :-
    rdf(Subject, _, Object, RDFGraph),
    rdf(Object, Prop, Value, RDFGraph).

% SPARQL-like pattern matching (simplified)
rdf_pattern_match(Subject, Object, pattern(Pattern), RDFGraph) :-
    Pattern =.. [Functor|Args],
    append(Args, [Subject, Object, RDFGraph], CallArgs),
    CallPattern =.. [Functor|CallArgs],
    call(CallPattern).

%% Path Evaluation and Querying %%

% Execute query based on a path expression with term evaluation
execute_d_path_query(PathString, SubjectType, ObjectType, Results) :-
    parse_d_path(PathString, Path),
    d_path(Source, Target, Morphisms) = Path,
    % Verify the path is valid in the category theory model
    compose_d_path(Source, Target, Morphisms),
    % Get the file path
    d_path_to_file_path(Path, FilePath),
    % Load the associated data
    load_ttl(FilePath),
    % Find subjects of the starting type
    rdf_subject_of_type(SubjectType, Subjects, FilePath),
    % For each subject, evaluate the path
    findall(Result,
            (
                member(Subject, Subjects),
                evaluate_path_recursively(Morphisms, Subject, Object, FilePath),
                % Verify object is of the target type
                rdf(Object, rdf:type, ObjectType, FilePath),
                Result = path_result(Subject, Object)
            ),
            Results).

% Generate valid paths between object types
generate_type_paths(SourceType, TargetType, MaxLength, RDFGraph, ValidPaths) :-
    % Find all objects of source and target types in the category model
    findall(Source,
            (
                d_object(Source, _),
                rdf_subject_of_type(Source, _, RDFGraph),
                Source = SourceType
            ),
            SourceObjects),
    findall(Target,
            (
                d_object(Target, _),
                rdf_subject_of_type(Target, _, RDFGraph),
                Target = TargetType
            ),
            TargetObjects),
    % For each source-target pair, find valid paths
    findall(Path,
            (
                member(Source, SourceObjects),
                member(Target, TargetObjects),
                generate_paths(Source, Target, MaxLength, Paths),
                member(Path, Paths),
                Path = d_path(_, _, Morphisms),
                check_path_validity(Source, Target, Morphisms, RDFGraph)
            ),
            ValidPaths).

%% Example Setup %%

% Define common Prolog predicates for RDF patterns
define_common_rdf_predicates :-
    % Direct relation: works_at(X, Y) :- rdf(X, ex:worksAt, Y)
    assert((works_at(X, Y, G) :- rdf(X, ex:worksAt, Y, G))),
    % Inverse relation: employer_of(X, Y) :- rdf(Y, ex:worksAt, X)
    assert((employer_of(X, Y, G) :- rdf(Y, ex:worksAt, X, G))),
    % Property path: works_in_city(X, Y) :- rdf(X, ex:worksAt, Z), rdf(Z, ex:locatedIn, Y)
    assert((works_in_city(X, Y, G) :- rdf(X, ex:worksAt, Z, G), rdf(Z, ex:locatedIn, Y, G))),
    % Transitive: part_of_org(X, Y) :- rdf_tc(X, ex:partOf, Y, G, [])
    assert((part_of_org(X, Y, G) :- rdf_tc(X, ex:partOf, Y, G, []))),
    % Property value: uses_tech(X, Y) :- rdf(X, ex:uses, Y), rdf(Y, ex:type, "Technology")
    assert((uses_tech(X, Y, G) :- rdf(X, ex:uses, Y, G), rdf(Y, ex:type, literal("Technology"), G))),
    format('Defined common RDF predicates~n').

% Example: Setting up a double category model
example_double_setup :-
    % Set up category
    define_double_category('knowledge'),
    define_common_rdf_predicates,

    % Define objects
    define_d_object('knowledge', 'person', 'persons'),
    define_d_object('knowledge', 'company', 'companies'),
    define_d_object('knowledge', 'product', 'products'),
    define_d_object('knowledge', 'location', 'locations'),
    define_d_object('knowledge', 'technology', 'technologies'),

    % Define double morphisms with both string labels and Prolog terms
    % Morphism: string label, Prolog term for RDF pattern
    define_d_morphism('knowledge', 'works_at', 'knowledge:person', 'knowledge:company',
                      'works_at', works_at),
    define_d_morphism('knowledge', 'employer_of', 'knowledge:company', 'knowledge:person',
                      'employs', employer_of),
    define_d_morphism('knowledge', 'located_in', 'knowledge:company', 'knowledge:location',
                      'located_in', rdf_direct_relation),
    define_d_morphism('knowledge', 'works_in', 'knowledge:person', 'knowledge:location',
                      'works_in', works_in_city),
    define_d_morphism('knowledge', 'produces', 'knowledge:company', 'knowledge:product',
                      'produces', rdf_direct_relation),
    define_d_morphism('knowledge', 'uses', 'knowledge:person', 'knowledge:product',
                      'uses', rdf_direct_relation),
    define_d_morphism('knowledge', 'built_with', 'knowledge:product', 'knowledge:technology',
                      'built_with', rdf_direct_relation),
    define_d_morphism('knowledge', 'knows_tech', 'knowledge:person', 'knowledge:technology',
                      'knows', rdf_direct_relation),

    % Create some example paths
    assert(d_path('knowledge:person', 'knowledge:location',
                ['knowledge:works_at', 'knowledge:located_in'])),
    assert(d_path('knowledge:person', 'knowledge:product',
                ['knowledge:works_at', 'knowledge:produces'])),
    assert(d_path('knowledge:person', 'knowledge:technology',
                ['knowledge:uses', 'knowledge:built_with'])),

    % Create example instance data for each path
    create_double_example_data,

    format('Example double category model setup complete.~n').

% Create example instance data with RDF triples
create_double_example_data :-
    % Path: persons -> works_at -> companies -> located_in -> locations
    Path1 = d_path('knowledge:person', 'knowledge:location',
                 ['knowledge:works_at', 'knowledge:located_in']),
    d_path_to_file_path(Path1, FilePath1),
    init_rdf_db(FilePath1),

    % Add type information
    rdf_assert(ex:john, rdf:type, 'knowledge:person', FilePath1),
    rdf_assert(ex:jane, rdf:type, 'knowledge:person', FilePath1),
    rdf_assert(ex:acme, rdf:type, 'knowledge:company', FilePath1),
    rdf_assert(ex:globex, rdf:type, 'knowledge:company', FilePath1),
    rdf_assert(ex:new_york, rdf:type, 'knowledge:location', FilePath1),
    rdf_assert(ex:san_francisco, rdf:type, 'knowledge:location', FilePath1),

    % Add relationship data
    rdf_assert(ex:john, ex:worksAt, ex:acme, FilePath1),
    rdf_assert(ex:jane, ex:worksAt, ex:globex, FilePath1),
    rdf_assert(ex:acme, ex:locatedIn, ex:new_york, FilePath1),
    rdf_assert(ex:globex, ex:locatedIn, ex:san_francisco, FilePath1),
    save_ttl(FilePath1),

    % Path: persons -> works_at -> companies -> produces -> products
    Path2 = d_path('knowledge:person', 'knowledge:product',
                 ['knowledge:works_at', 'knowledge:produces']),
    d_path_to_file_path(Path2, FilePath2),
    init_rdf_db(FilePath2),

    % Add type information
    rdf_assert(ex:john, rdf:type, 'knowledge:person', FilePath2),
    rdf_assert(ex:jane, rdf:type, 'knowledge:person', FilePath2),
    rdf_assert(ex:acme, rdf:type, 'knowledge:company', FilePath2),
    rdf_assert(ex:globex, rdf:type, 'knowledge:company', FilePath2),
    rdf_assert(ex:widget_a, rdf:type, 'knowledge:product', FilePath2),
    rdf_assert(ex:widget_b, rdf:type, 'knowledge:product', FilePath2),

    % Add relationship data
    rdf_assert(ex:john, ex:worksAt, ex:acme, FilePath2),
    rdf_assert(ex:jane, ex:worksAt, ex:globex, FilePath2),
    rdf_assert(ex:acme, ex:produces, ex:widget_a, FilePath2),
    rdf_assert(ex:globex, ex:produces, ex:widget_b, FilePath2),
    save_ttl(FilePath2),

    % Path: persons -> uses -> products -> built_with -> technologies
    Path3 = d_path('knowledge:person', 'knowledge:technology',
                 ['knowledge:uses', 'knowledge:built_with']),
    d_path_to_file_path(Path3, FilePath3),
    init_rdf_db(FilePath3),

    % Add type information
    rdf_assert(ex:john, rdf:type, 'knowledge:person', FilePath3),
    rdf_assert(ex:jane, rdf:type, 'knowledge:person', FilePath3),
    rdf_assert(ex:laptop, rdf:type, 'knowledge:product', FilePath3),
    rdf_assert(ex:smartphone, rdf:type, 'knowledge:product', FilePath3),
    rdf_assert(ex:linux, rdf:type, 'knowledge:technology', FilePath3),
    rdf_assert(ex:android, rdf:type, 'knowledge:technology', FilePath3),

    % Add relationship data
    rdf_assert(ex:john, ex:uses, ex:laptop, FilePath3),
    rdf_assert(ex:jane, ex:uses, ex:smartphone, FilePath3),
    rdf_assert(ex:laptop, ex:built_with, ex:linux, FilePath3),
    rdf_assert(ex:smartphone, ex:built_with, ex:android, FilePath3),
    save_ttl(FilePath3),

    format('Example RDF data created for paths.~n').

%% Demo Queries %%

% Demo for executing a path query with term evaluation
demo_path_query :-
    PathString = 'knowledge:person.works_at.located_in',
    SourceType = 'knowledge:person',
    TargetType = 'knowledge:location',
    execute_d_path_query(PathString, SourceType, TargetType, Results),
    format('Results for path query ~w:~n', [PathString]),
    forall(
        member(path_result(Subject, Object), Results),
        format('  ~w -> ~w~n', [Subject, Object])
    ).

% Demo for generating valid paths between types
demo_generate_paths :-
    SourceType = 'knowledge:person',
    TargetType = 'knowledge:technology',
    MaxLength = 3,
    RDFGraph = 'persons/uses/built_with/technologies',
    load_ttl(RDFGraph),
    generate_type_paths(SourceType, TargetType, MaxLength, RDFGraph, ValidPaths),
    format('Valid paths from ~w to ~w (max length ~w):~n', [SourceType, TargetType, MaxLength]),
    forall(
        member(Path, ValidPaths),
        (
            Path = d_path(S, T, Morphisms),
            format('  Path: ~w -> ~w via ~w~n', [S, T, Morphisms])
        )
    ).

% Usage examples:
% ?- example_double_setup.
% ?- demo_path_query.
% ?- demo_generate_paths.
% ?- parse_d_path('knowledge:person.works_at.produces', Path), d_path_to_file_path(Path, FilePath).
% ?- execute_d_path_query('knowledge:person.works_at.produces', 'knowledge:person', 'knowledge:product', Results).
% ?- generate_paths('knowledge:person', 'knowledge:technology', 3, Paths).
