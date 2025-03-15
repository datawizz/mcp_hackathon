%% Prolog WASM Interface for Category Theory-Based RDF Knowledge Engine
%% This module extends the core functionality with WASM-friendly interfaces
%% Date: March 14, 2025

:- module(cat_rdf_wasm, [
    % JSON serialization for WASM interface
    json_response/2,
    json_error/3,

    % WASM-friendly CRUD operations for Facts
    wasm_add_rdf_fact/4,
    wasm_add_gold_fact/3,
    wasm_query_rdf_facts/5,
    wasm_update_rdf_fact/8,
    wasm_delete_rdf_fact/4,
    wasm_delete_gold_fact/3,

    % WASM-friendly CRUD operations for Rules/Morphisms
    wasm_create_category/1,
    wasm_add_category_object/3,
    wasm_add_morphism/6,
    wasm_add_gold_constraint/3,
    wasm_get_morphisms/2,
    wasm_delete_morphism/2,
    wasm_delete_gold_constraint/2,

    % WASM-friendly Predicate Discovery
    wasm_parse_path/2,
    wasm_generate_paths/4,
    wasm_find_valid_paths/5,
    wasm_execute_path_query/5,
    wasm_check_path_consistency/2,
    wasm_evaluate_accuracy/2
]).

:- use_module(library(http/json)).
:- use_module(cat_rdf).  % Import the core module

%% =============================================
%% JSON Serialization for WASM Interface
%% =============================================

% Generate a success JSON response
json_response(Data, JsonString) :-
    atom_json_dict(JsonString, _{success: true, data: Data}, []).

% Generate an error JSON response
json_error(Code, Message, JsonString) :-
    atom_json_dict(JsonString, _{success: false, code: Code, message: Message}, []).

%% =============================================
%% WASM-friendly CRUD Operations for Facts
%% =============================================

% Add an RDF triple fact
wasm_add_rdf_fact(Subject, Predicate, Object, Response) :-
    catch(
        (
            rdf_assert(Subject, Predicate, Object),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Add a gold standard fact
wasm_add_gold_fact(Predicate, Entity, TruthValue, Response) :-
    catch(
        (
            atom_string(PredAtom, Predicate),
            atom_string(EntityAtom, Entity),
            define_gold_fact(PredAtom, EntityAtom, TruthValue),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Query RDF facts
wasm_query_rdf_facts(Subject, Predicate, Object, Graph, Response) :-
    catch(
        (
            % Handle optional parameters (null or empty means wildcard)
            (Subject = null -> SubjectVar = S ; atom_string(SubjectVar, Subject)),
            (Predicate = null -> PredicateVar = P ; atom_string(PredicateVar, Predicate)),
            (Object = null -> ObjectVar = O ; atom_string(ObjectVar, Object)),
            (Graph = null -> GraphVar = G ; atom_string(GraphVar, Graph)),

            findall(
                _{subject: S, predicate: P, object: O, graph: G},
                rdf(SubjectVar, PredicateVar, ObjectVar, GraphVar),
                Results
            ),
            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Update an RDF fact
wasm_update_rdf_fact(OldSubject, OldPredicate, OldObject, OldGraph,
                    NewSubject, NewPredicate, NewObject, NewGraph, Response) :-
    catch(
        (
            % Convert strings to atoms
            atom_string(OldSubjectAtom, OldSubject),
            atom_string(OldPredicateAtom, OldPredicate),
            atom_string(OldObjectAtom, OldObject),
            atom_string(OldGraphAtom, OldGraph),
            atom_string(NewSubjectAtom, NewSubject),
            atom_string(NewPredicateAtom, NewPredicate),
            atom_string(NewObjectAtom, NewObject),
            atom_string(NewGraphAtom, NewGraph),

            % Update by removing old and adding new
            rdf_retractall(OldSubjectAtom, OldPredicateAtom, OldObjectAtom, OldGraphAtom),
            rdf_assert(NewSubjectAtom, NewPredicateAtom, NewObjectAtom, NewGraphAtom),

            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Delete an RDF fact
wasm_delete_rdf_fact(Subject, Predicate, Object, Response) :-
    catch(
        (
            atom_string(SubjectAtom, Subject),
            atom_string(PredicateAtom, Predicate),
            atom_string(ObjectAtom, Object),
            rdf_retractall(SubjectAtom, PredicateAtom, ObjectAtom, _),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Delete a gold standard fact
wasm_delete_gold_fact(Predicate, Entity, Response) :-
    catch(
        (
            atom_string(PredicateAtom, Predicate),
            atom_string(EntityAtom, Entity),
            retractall(gold_fact(PredicateAtom, EntityAtom, _)),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

%% =============================================
%% WASM-friendly CRUD Operations for Rules/Morphisms
%% =============================================

% Create a new category
wasm_create_category(CategoryName, Response) :-
    catch(
        (
            atom_string(CategoryAtom, CategoryName),
            define_double_category(CategoryAtom),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Add a category object
wasm_add_category_object(Category, ObjectId, Label, Response) :-
    catch(
        (
            atom_string(CategoryAtom, Category),
            atom_string(ObjectIdAtom, ObjectId),
            atom_string(LabelAtom, Label),
            define_d_object(CategoryAtom, ObjectIdAtom, LabelAtom),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Add a morphism
wasm_add_morphism(Category, MorphismId, SourceObj, TargetObj, StringLabel, PrologTermString, Response) :-
    catch(
        (
            atom_string(CategoryAtom, Category),
            atom_string(MorphismIdAtom, MorphismId),
            atom_string(SourceObjAtom, SourceObj),
            atom_string(TargetObjAtom, TargetObj),
            atom_string(StringLabelAtom, StringLabel),

            % Convert the term string to an actual Prolog term
            term_string(PrologTerm, PrologTermString),

            define_d_morphism(CategoryAtom, MorphismIdAtom, SourceObjAtom, TargetObjAtom, StringLabelAtom, PrologTerm),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Add a gold constraint
wasm_add_gold_constraint(SourcePred, TargetPred, ConstraintType, Response) :-
    catch(
        (
            atom_string(SourcePredAtom, SourcePred),
            atom_string(TargetPredAtom, TargetPred),
            atom_string(ConstraintTypeAtom, ConstraintType),
            define_gold_constraint(SourcePredAtom, TargetPredAtom, ConstraintTypeAtom),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Get all morphisms for a category
wasm_get_morphisms(CategoryName, Response) :-
    catch(
        (
            atom_string(CategoryAtom, CategoryName),
            findall(
                _{
                    category: CategoryAtom,
                    morphismId: M,
                    sourceObj: S,
                    targetObj: T,
                    stringLabel: L,
                    prologTermString: PTS
                },
                (
                    d_morphism(CategoryAtom, M, S, T, L, P),
                    term_string(P, PTS)
                ),
                Results
            ),
            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Delete a morphism
wasm_delete_morphism(Category, MorphismId, Response) :-
    catch(
        (
            atom_string(CategoryAtom, Category),
            atom_string(MorphismIdAtom, MorphismId),
            retractall(d_morphism(CategoryAtom, MorphismIdAtom, _, _, _, _)),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Delete a gold constraint
wasm_delete_gold_constraint(SourcePred, TargetPred, Response) :-
    catch(
        (
            atom_string(SourcePredAtom, SourcePred),
            atom_string(TargetPredAtom, TargetPred),
            retractall(gold_constraint(SourcePredAtom, TargetPredAtom, _)),
            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

%% =============================================
%% WASM-friendly Predicate Discovery
%% =============================================

% Parse a path expression
wasm_parse_path(PathExpression, Response) :-
    catch(
        (
            atom_string(PathExpressionAtom, PathExpression),
            parse_d_path(PathExpressionAtom, Path),
            json_response(_{morphisms: Path}, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Generate all possible paths
wasm_generate_paths(SourceObj, TargetObj, MaxLength, Response) :-
    catch(
        (
            atom_string(SourceObjAtom, SourceObj),
            atom_string(TargetObjAtom, TargetObj),
            generate_paths(SourceObjAtom, TargetObjAtom, MaxLength, Paths),
            maplist(path_to_dict, Paths, PathDicts),
            json_response(PathDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Convert a path to a dictionary
path_to_dict(Path, _{morphisms: Path}).

% Find valid paths in RDF data
wasm_find_valid_paths(SourceEntity, TargetEntity, MaxLength, GraphName, Response) :-
    catch(
        (
            atom_string(SourceEntityAtom, SourceEntity),
            atom_string(TargetEntityAtom, TargetEntity),
            atom_string(GraphNameAtom, GraphName),
            find_valid_rdf_paths(SourceEntityAtom, TargetEntityAtom, MaxLength, GraphNameAtom, ValidPaths),
            maplist(path_to_dict, ValidPaths, PathDicts),
            json_response(PathDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Execute a path query with constraints
wasm_execute_path_query(PathExpression, SubjectType, ObjectType, IncludeConfidence, Response) :-
    catch(
        (
            atom_string(PathExpressionAtom, PathExpression),
            atom_string(SubjectTypeAtom, SubjectType),
            atom_string(ObjectTypeAtom, ObjectType),

            % Get all gold constraints
            findall(constraint(S, T, Type), gold_constraint(S, T, Type), GoldConstraints),

            execute_d_path_query_with_constraints(
                PathExpressionAtom,
                SubjectTypeAtom,
                ObjectTypeAtom,
                GoldConstraints,
                Results,
                ConfidenceScore
            ),

            % Format results based on whether confidence is requested
            (IncludeConfidence = true ->
                findall(
                    _{subject: S, object: O, confidence: ConfidenceScore},
                    member((S, O), Results),
                    ResultDicts
                )
            ;
                findall(
                    _{subject: S, object: O},
                    member((S, O), Results),
                    ResultDicts
                )
            ),

            json_response(ResultDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Check path consistency
wasm_check_path_consistency(PathsJson, Response) :-
    catch(
        (
            % Parse paths from JSON
            atom_json_dict(PathsJson, PathDicts, []),
            maplist(dict_to_path, PathDicts, Paths),

            findall(constraint(S, T, implication), gold_constraint(S, T, implication), GoldConstraints),
            check_path_consistency(Paths, GoldConstraints, ViolationMetric),
            ConsistencyScore is 1 - ViolationMetric,

            json_response(ConsistencyScore, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Convert a path dictionary to a path
dict_to_path(Dict, Path) :-
    get_dict(morphisms, Dict, Path).

% Evaluate accuracy
wasm_evaluate_accuracy(PredictionsJson, Response) :-
    catch(
        (
            % Parse predictions from JSON
            atom_json_dict(PredictionsJson, PredictionDicts, []),
            maplist(dict_to_prediction, PredictionDicts, Predictions),

            findall(gold(P, E, T), gold_fact(P, E, T), GoldFacts),
            evaluate_accuracy(Predictions, GoldFacts, F1Score),

            json_response(F1Score, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Convert a prediction dictionary to a prediction
dict_to_prediction(Dict, pred(Pred, Entity, TruthValue)) :-
    get_dict(predicate, Dict, PredStr),
    get_dict(entity, Dict, EntityStr),
    get_dict(truthValue, Dict, TruthValue),
    atom_string(Pred, PredStr),
    atom_string(Entity, EntityStr).

%% =============================================
%% Entry Point for WASM (Example)
%% =============================================

% Example of a WASM entry function that could be called from TypeScript
wasm_main(Command, ArgsJson, Response) :-
    catch(
        (
            % Parse arguments from JSON
            atom_json_dict(ArgsJson, Args, []),

            % Dispatch to appropriate handler based on command
            (Command = "add_rdf_fact" ->
                get_dict(subject, Args, Subject),
                get_dict(predicate, Args, Predicate),
                get_dict(object, Args, Object),
                wasm_add_rdf_fact(Subject, Predicate, Object, Response)
            ;
            Command = "query_rdf_facts" ->
                get_dict(subject, Args, Subject),
                get_dict(predicate, Args, Predicate),
                get_dict(object, Args, Object),
                get_dict(graph, Args, Graph),
                wasm_query_rdf_facts(Subject, Predicate, Object, Graph, Response)
            ;
            % Add more command handlers as needed
            json_error(400, "Unknown command", Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).
