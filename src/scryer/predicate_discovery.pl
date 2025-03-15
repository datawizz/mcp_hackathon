%% Prolog Implementation for Predicate-Focused API
%% Based on the Category Theory-Based RDF Knowledge Engine with Gold Standard Evaluation
%% Date: March 14, 2025

:- module(cat_rdf_predicate, [
    % Predicate definition management
    wasm_create_predicate/7,
    wasm_get_predicate/1,
    wasm_update_predicate/2,
    wasm_delete_predicate/1,
    wasm_get_all_predicates/2,
    wasm_batch_predicate_operation/1,

    % Predicate instance management
    wasm_add_predicate_instance/5,
    wasm_query_predicate_instances/1,
    wasm_update_predicate_instance/2,
    wasm_delete_predicate_instance/1,
    wasm_batch_predicate_instance_operation/1,

    % Predicate set management
    wasm_create_predicate_set/1,
    wasm_get_predicate_set/1,
    wasm_update_predicate_set/2,
    wasm_delete_predicate_set/1,
    wasm_get_all_predicate_sets/0,
    wasm_add_predicates_to_set/2,
    wasm_remove_predicates_from_set/2,

    % Predicate path management
    wasm_create_predicate_path/1,
    wasm_parse_path_expression/1,
    wasm_generate_paths/3,
    wasm_execute_path_query/2,

    % Gold standard evaluation
    wasm_define_gold_fact/1,
    wasm_define_logical_constraint/1,
    wasm_create_constraint_graph/1,
    wasm_evaluate_accuracy/2,
    wasm_measure_consistency/2,

    % Predicate discovery
    wasm_discover_potential_predicates/1,
    wasm_discover_path_patterns/2,
    wasm_find_missing_predicates/1,
    wasm_suggest_constraint_candidates/1,
    wasm_complete_missing_predicates/2
]).

:- use_module(library(http/json)).
:- use_module(cat_rdf).  % Import the core module

%% =============================================
%% Dynamic Predicates for Data Storage
%% =============================================

% Dynamic predicates for predicate definitions
:- dynamic predicate_def/7.  % URI, Label, Domain, Range, Inverse, Transitive, Symmetric

% Dynamic predicates for predicate sets
:- dynamic predicate_set/3.  % ID, Name, Description
:- dynamic predicate_set_member/2.  % SetID, PredicateURI

% Dynamic predicates for predicate paths
:- dynamic predicate_path/4.  % Path, Label, SourceType, TargetType

% Dynamic predicates for constraint graphs
:- dynamic constraint_graph/2.  % ID, Description
:- dynamic constraint_graph_member/2.  % GraphID, ConstraintID

%% =============================================
%% JSON Helpers
%% =============================================

% Generate JSON response
json_response(Data, JsonString) :-
    atom_json_dict(JsonString, _{success: true, data: Data}, []).

% Generate JSON error
json_error(Code, Message, JsonString) :-
    atom_json_dict(JsonString, _{success: false, code: Code, message: Message}, []).

%% =============================================
%% Predicate Definition Management
%% =============================================

% Create a new predicate definition
wasm_create_predicate(URI, Label, Domain, Range, Inverse, Transitive, Symmetric, Response) :-
    catch(
        (
            % Convert string values to atoms
            atom_string(URIAtom, URI),
            atom_string(LabelAtom, Label),
            atom_string(DomainAtom, Domain),
            atom_string(RangeAtom, Range),
            (Inverse \= null -> atom_string(InverseAtom, Inverse) ; InverseAtom = null),

            % Store the predicate definition
            assertz(predicate_def(URIAtom, LabelAtom, DomainAtom, RangeAtom, InverseAtom, Transitive, Symmetric)),

            % Create response
            PredicateDict = _{
                uri: URI,
                label: Label,
                domain: Domain,
                range: Range,
                inverse: Inverse,
                transitive: Transitive,
                symmetric: Symmetric
            },
            json_response(PredicateDict, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Retrieve a predicate by URI
wasm_get_predicate(URI, Response) :-
    catch(
        (
            atom_string(URIAtom, URI),

            % Find the predicate
            (predicate_def(URIAtom, Label, Domain, Range, Inverse, Transitive, Symmetric) ->
                % Create response
                PredicateDict = _{
                    uri: URI,
                    label: Label,
                    domain: Domain,
                    range: Range,
                    inverse: Inverse,
                    transitive: Transitive,
                    symmetric: Symmetric
                },
                json_response(PredicateDict, Response)
            ;
                % Predicate not found
                json_error(404, 'Predicate not found', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Update an existing predicate
wasm_update_predicate(URI, UpdatesJson, Response) :-
    catch(
        (
            atom_string(URIAtom, URI),

            % Parse updates from JSON
            atom_json_dict(UpdatesJson, Updates, []),

            % Check if predicate exists
            (predicate_def(URIAtom, OldLabel, OldDomain, OldRange, OldInverse, OldTransitive, OldSymmetric) ->
                % Get updated values (or use old ones if not specified)
                (get_dict(label, Updates, NewLabel) -> true ; atom_string(NewLabel, OldLabel)),
                (get_dict(domain, Updates, NewDomain) -> true ; atom_string(NewDomain, OldDomain)),
                (get_dict(range, Updates, NewRange) -> true ; atom_string(NewRange, OldRange)),
                (get_dict(inverse, Updates, NewInverse) -> true ; NewInverse = OldInverse),
                (get_dict(transitive, Updates, NewTransitive) -> true ; NewTransitive = OldTransitive),
                (get_dict(symmetric, Updates, NewSymmetric) -> true ; NewSymmetric = OldSymmetric),

                % Update predicate
                retractall(predicate_def(URIAtom, _, _, _, _, _, _)),
                assertz(predicate_def(URIAtom, NewLabel, NewDomain, NewRange, NewInverse, NewTransitive, NewSymmetric)),

                % Create response
                PredicateDict = _{
                    uri: URI,
                    label: NewLabel,
                    domain: NewDomain,
                    range: NewRange,
                    inverse: NewInverse,
                    transitive: NewTransitive,
                    symmetric: NewSymmetric
                },
                json_response(PredicateDict, Response)
            ;
                % Predicate not found
                json_error(404, 'Predicate not found', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Delete a predicate
wasm_delete_predicate(URI, Response) :-
    catch(
        (
            atom_string(URIAtom, URI),

            % Check if predicate exists
            (predicate_def(URIAtom, _, _, _, _, _, _) ->
                % Delete predicate
                retractall(predicate_def(URIAtom, _, _, _, _, _, _)),

                % Also remove from any predicate sets
                retractall(predicate_set_member(_, URIAtom)),

                json_response(true, Response)
            ;
                % Predicate not found
                json_error(404, 'Predicate not found', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Retrieve all predicates, optionally filtered
wasm_get_all_predicates(DomainFilter, RangeFilter, Response) :-
    catch(
        (
            % Apply filters if provided
            findall(
                _{
                    uri: URI,
                    label: Label,
                    domain: Domain,
                    range: Range,
                    inverse: Inverse,
                    transitive: Transitive,
                    symmetric: Symmetric
                },
                (
                    predicate_def(URIAtom, LabelAtom, DomainAtom, RangeAtom, InverseAtom, Transitive, Symmetric),
                    % Apply domain filter if provided
                    (DomainFilter \= null -> DomainFilter = DomainAtom ; true),
                    % Apply range filter if provided
                    (RangeFilter \= null -> RangeFilter = RangeAtom ; true),
                    % Convert atoms to strings for JSON
                    atom_string(URIAtom, URI),
                    atom_string(LabelAtom, Label),
                    atom_string(DomainAtom, Domain),
                    atom_string(RangeAtom, Range),
                    (InverseAtom \= null -> atom_string(InverseAtom, Inverse) ; Inverse = null)
                ),
                Predicates
            ),

            json_response(Predicates, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Perform batch operations on predicates
wasm_batch_predicate_operation(BatchJson, Response) :-
    catch(
        (
            % Parse batch from JSON
            atom_json_dict(BatchJson, Batch, []),

            % Extract batch details
            get_dict(predicates, Batch, PredicateURIs),
            get_dict(operation, Batch, Operation),

            % Process based on operation type
            (Operation = "create" ->
                % Get data for creation
                get_dict(data, Batch, Data),
                % Create each predicate
                maplist(create_predicate_from_data(Data), PredicateURIs),
                json_response(true, Response)
            ; Operation = "update" ->
                % Get data for update
                get_dict(data, Batch, Data),
                % Update each predicate
                maplist(update_predicate_from_data(Data), PredicateURIs),
                json_response(true, Response)
            ; Operation = "delete" ->
                % Delete each predicate
                maplist(delete_predicate, PredicateURIs),
                json_response(true, Response)
            ;
                % Invalid operation
                json_error(400, 'Invalid batch operation', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Helper for batch create
create_predicate_from_data(Data, URI) :-
    atom_string(URIAtom, URI),
    get_dict_default(label, Data, "Unnamed Predicate", Label),
    get_dict_default(domain, Data, "owl:Thing", Domain),
    get_dict_default(range, Data, "owl:Thing", Range),
    get_dict_default(inverse, Data, null, Inverse),
    get_dict_default(transitive, Data, false, Transitive),
    get_dict_default(symmetric, Data, false, Symmetric),

    assertz(predicate_def(URIAtom, Label, Domain, Range, Inverse, Transitive, Symmetric)).

% Helper for batch update
update_predicate_from_data(Data, URI) :-
    atom_string(URIAtom, URI),
    predicate_def(URIAtom, OldLabel, OldDomain, OldRange, OldInverse, OldTransitive, OldSymmetric),

    get_dict_default(label, Data, OldLabel, NewLabel),
    get_dict_default(domain, Data, OldDomain, NewDomain),
    get_dict_default(range, Data, OldRange, NewRange),
    get_dict_default(inverse, Data, OldInverse, NewInverse),
    get_dict_default(transitive, Data, OldTransitive, NewTransitive),
    get_dict_default(symmetric, Data, OldSymmetric, NewSymmetric),

    retractall(predicate_def(URIAtom, _, _, _, _, _, _)),
    assertz(predicate_def(URIAtom, NewLabel, NewDomain, NewRange, NewInverse, NewTransitive, NewSymmetric)).

% Helper for batch delete
delete_predicate(URI) :-
    atom_string(URIAtom, URI),
    retractall(predicate_def(URIAtom, _, _, _, _, _, _)),
    retractall(predicate_set_member(_, URIAtom)).

% Helper to get dict value with default
get_dict_default(Key, Dict, Default, Value) :-
    (get_dict(Key, Dict, Value) -> true ; Value = Default).

%% =============================================
%% Predicate Instance Management
%% =============================================

% Add a predicate instance (RDF triple)
wasm_add_predicate_instance(Subject, Predicate, Object, Confidence, Graph, Response) :-
    catch(
        (
            % Convert strings to atoms
            atom_string(SubjectAtom, Subject),
            atom_string(PredicateAtom, Predicate),
            atom_string(ObjectAtom, Object),
            atom_string(GraphAtom, Graph),

            % Check if predicate exists in our definitions
            (predicate_def(PredicateAtom, _, _, _, _, _, _) ->
                % Add the RDF triple
                rdf_assert(SubjectAtom, PredicateAtom, ObjectAtom, GraphAtom),

                % If we're tracking confidence, store it (custom predicate)
                (Confidence < 1.0 ->
                    assertz(triple_confidence(SubjectAtom, PredicateAtom, ObjectAtom, GraphAtom, Confidence))
                ; true),

                % Create response
                InstanceDict = _{
                    subject: Subject,
                    predicate: Predicate,
                    object: Object,
                    confidence: Confidence,
                    graph: Graph
                },
                json_response(InstanceDict, Response)
            ;
                % Predicate not found
                json_error(404, 'Predicate not defined', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Query for predicate instances
wasm_query_predicate_instances(ParamsJson, Response) :-
    catch(
        (
            % Parse params from JSON
            atom_json_dict(ParamsJson, Params, []),

            % Extract query parameters with defaults
            get_dict_default(subject, Params, null, SubjectParam),
            get_dict_default(predicate, Params, null, PredicateParam),
            get_dict_default(object, Params, null, ObjectParam),
            get_dict_default(graph, Params, null, GraphParam),
            get_dict_default(subjectType, Params, null, SubjectTypeParam),
            get_dict_default(objectType, Params, null, ObjectTypeParam),
            get_dict_default(minConfidence, Params, 0.0, MinConfidence),
            get_dict_default(limit, Params, 100, Limit),
            get_dict_default(offset, Params, 0, Offset),

            % Convert params to variables or atoms for query
            param_to_var_or_atom(SubjectParam, Subject),
            param_to_var_or_atom(PredicateParam, Predicate),
            param_to_var_or_atom(ObjectParam, Object),
            param_to_var_or_atom(GraphParam, Graph),

            % Build query based on provided parameters
            findall(
                _{
                    subject: SubjectStr,
                    predicate: PredicateStr,
                    object: ObjectStr,
                    confidence: Confidence,
                    graph: GraphStr
                },
                (
                    % Base triple pattern
                    rdf(Subject, Predicate, Object, Graph),

                    % Additional type constraints if specified
                    (SubjectTypeParam \= null -> rdf(Subject, rdf:type, SubjectType), atom_string(SubjectType, SubjectTypeParam) ; true),
                    (ObjectTypeParam \= null -> rdf(Object, rdf:type, ObjectType), atom_string(ObjectType, ObjectTypeParam) ; true),

                    % Get confidence if available
                    (triple_confidence(Subject, Predicate, Object, Graph, TripleConfidence) ->
                        Confidence = TripleConfidence
                    ;
                        Confidence = 1.0
                    ),

                    % Apply confidence threshold
                    Confidence >= MinConfidence,

                    % Convert to strings for JSON
                    atom_string(Subject, SubjectStr),
                    atom_string(Predicate, PredicateStr),
                    atom_string(Object, ObjectStr),
                    atom_string(Graph, GraphStr)
                ),
                AllResults
            ),

            % Count total results
            length(AllResults, TotalCount),

            % Apply pagination
            apply_pagination(AllResults, Offset, Limit, PagedResults),
            length(PagedResults, ResultCount),
            HasMore = (ResultCount + Offset < TotalCount),

            % Create paged results
            ResultsDict = _{
                items: PagedResults,
                totalCount: TotalCount,
                pageIndex: Offset // Limit,
                pageSize: Limit,
                hasMore: HasMore
            },

            json_response(ResultsDict, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Update a predicate instance
wasm_update_predicate_instance(OldJson, NewJson, Response) :-
    catch(
        (
            % Parse old and new instances from JSON
            atom_json_dict(OldJson, OldInstance, []),
            atom_json_dict(NewJson, NewInstance, []),

            % Extract instance details
            get_dict(subject, OldInstance, OldSubject),
            get_dict(predicate, OldInstance, OldPredicate),
            get_dict(object, OldInstance, OldObject),
            get_dict_default(graph, OldInstance, "user", OldGraph),

            get_dict(subject, NewInstance, NewSubject),
            get_dict(predicate, NewInstance, NewPredicate),
            get_dict(object, NewInstance, NewObject),
            get_dict_default(graph, NewInstance, "user", NewGraph),
            get_dict_default(confidence, NewInstance, 1.0, NewConfidence),

            % Convert to atoms
            atom_string(OldSubjectAtom, OldSubject),
            atom_string(OldPredicateAtom, OldPredicate),
            atom_string(OldObjectAtom, OldObject),
            atom_string(OldGraphAtom, OldGraph),

            atom_string(NewSubjectAtom, NewSubject),
            atom_string(NewPredicateAtom, NewPredicate),
            atom_string(NewObjectAtom, NewObject),
            atom_string(NewGraphAtom, NewGraph),

            % Remove old triple
            rdf_retractall(OldSubjectAtom, OldPredicateAtom, OldObjectAtom, OldGraphAtom),
            retractall(triple_confidence(OldSubjectAtom, OldPredicateAtom, OldObjectAtom, OldGraphAtom, _)),

            % Add new triple
            rdf_assert(NewSubjectAtom, NewPredicateAtom, NewObjectAtom, NewGraphAtom),

            % Store confidence if needed
            (NewConfidence < 1.0 ->
                assertz(triple_confidence(NewSubjectAtom, NewPredicateAtom, NewObjectAtom, NewGraphAtom, NewConfidence))
            ; true),

            json_response(NewInstance, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Delete a predicate instance
wasm_delete_predicate_instance(InstanceJson, Response) :-
    catch(
        (
            % Parse instance from JSON
            atom_json_dict(InstanceJson, Instance, []),

            % Extract instance details
            get_dict(subject, Instance, Subject),
            get_dict(predicate, Instance, Predicate),
            get_dict(object, Instance, Object),
            get_dict_default(graph, Instance, "user", Graph),

            % Convert to atoms
            atom_string(SubjectAtom, Subject),
            atom_string(PredicateAtom, Predicate),
            atom_string(ObjectAtom, Object),
            atom_string(GraphAtom, Graph),

            % Remove triple
            rdf_retractall(SubjectAtom, PredicateAtom, ObjectAtom, GraphAtom),
            retractall(triple_confidence(SubjectAtom, PredicateAtom, ObjectAtom, GraphAtom, _)),

            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Perform batch operations on predicate instances
wasm_batch_predicate_instance_operation(BatchJson, Response) :-
    catch(
        (
            % Parse batch from JSON
            atom_json_dict(BatchJson, Batch, []),

            % Extract batch details
            get_dict(instances, Batch, Instances),
            get_dict(operation, Batch, Operation),

            % Process based on operation type
            (Operation = "create" ->
                % Add each instance
                maplist(add_predicate_instance, Instances),
                json_response(true, Response)
            ; Operation = "update" ->
                % For update, we need pairs of old/new instances
                (length(Instances, Len), Len mod 2 =:= 0 ->
                    % Group instances into pairs
                    group_pairs(Instances, Pairs),
                    % Update each pair
                    maplist(update_predicate_instance_pair, Pairs),
                    json_response(true, Response)
                ;
                    json_error(400, 'Update requires pairs of instances', Response)
                )
            ; Operation = "delete" ->
                % Delete each instance
                maplist(delete_predicate_instance, Instances),
                json_response(true, Response)
            ;
                % Invalid operation
                json_error(400, 'Invalid batch operation', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Helper to convert parameter to variable or atom
param_to_var_or_atom(null, _).  % If null, leave as variable
param_to_var_or_atom(Param, Atom) :-
    atom_string(Atom, Param).

% Helper to apply pagination
apply_pagination(List, Offset, Limit, Result) :-
    length(List, TotalLen),
    (Offset >= TotalLen ->
        Result = []
    ;
        drop(Offset, List, DroppedList),
        take(Limit, DroppedList, Result)
    ).

% Helper to drop N elements from a list
drop(0, List, List) :- !.
drop(_, [], []) :- !.
drop(N, [_|Tail], Result) :-
    N > 0,
    N1 is N - 1,
    drop(N1, Tail, Result).

% Helper to take N elements from a list
take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [Head|Tail], [Head|Result]) :-
    N > 0,
    N1 is N - 1,
    take(N1, Tail, Result).

% Helper to group list into pairs
group_pairs([], []).
group_pairs([A, B|Rest], [[A, B]|Pairs]) :-
    group_pairs(Rest, Pairs).

% Helper to add a predicate instance
add_predicate_instance(Instance) :-
    get_dict(subject, Instance, Subject),
    get_dict(predicate, Instance, Predicate),
    get_dict(object, Instance, Object),
    get_dict_default(graph, Instance, "user", Graph),
    get_dict_default(confidence, Instance, 1.0, Confidence),

    atom_string(SubjectAtom, Subject),
    atom_string(PredicateAtom, Predicate),
    atom_string(ObjectAtom, Object),
    atom_string(GraphAtom, Graph),

    rdf_assert(SubjectAtom, PredicateAtom, ObjectAtom, GraphAtom),

    (Confidence < 1.0 ->
        assertz(triple_confidence(SubjectAtom, PredicateAtom, ObjectAtom, GraphAtom, Confidence))
    ; true).

% Helper to update a predicate instance pair
update_predicate_instance_pair([Old, New]) :-
    get_dict(subject, Old, OldSubject),
    get_dict(predicate, Old, OldPredicate),
    get_dict(object, Old, OldObject),
    get_dict_default(graph, Old, "user", OldGraph),

    get_dict(subject, New, NewSubject),
    get_dict(predicate, New, NewPredicate),
    get_dict(object, New, NewObject),
    get_dict_default(graph, New, "user", NewGraph),
    get_dict_default(confidence, New, 1.0, NewConfidence),

    atom_string(OldSubjectAtom, OldSubject),
    atom_string(OldPredicateAtom, OldPredicate),
    atom_string(OldObjectAtom, OldObject),
    atom_string(OldGraphAtom, OldGraph),

    atom_string(NewSubjectAtom, NewSubject),
    atom_string(NewPredicateAtom, NewPredicate),
    atom_string(NewObjectAtom, NewObject),
    atom_string(NewGraphAtom, NewGraph),

    rdf_retractall(OldSubjectAtom, OldPredicateAtom, OldObjectAtom, OldGraphAtom),
    retractall(triple_confidence(OldSubjectAtom, OldPredicateAtom, OldObjectAtom, OldGraphAtom, _)),

    rdf_assert(NewSubjectAtom, NewPredicateAtom, NewObjectAtom, NewGraphAtom),

    (NewConfidence < 1.0 ->
        assertz(triple_confidence(NewSubjectAtom, NewPredicateAtom, NewObjectAtom, NewGraphAtom, NewConfidence))
    ; true).

% Helper to delete a predicate instance
delete_predicate_instance(Instance) :-
    get_dict(subject, Instance, Subject),
    get_dict(predicate, Instance, Predicate),
    get_dict(object, Instance, Object),
    get_dict_default(graph, Instance, "user", Graph),

    atom_string(SubjectAtom, Subject),
    atom_string(PredicateAtom, Predicate),
    atom_string(ObjectAtom, Object),
    atom_string(GraphAtom, Graph),

    rdf_retractall(SubjectAtom, PredicateAtom, ObjectAtom, GraphAtom),
    retractall(triple_confidence(SubjectAtom, PredicateAtom, ObjectAtom, GraphAtom, _)).

%% =============================================
%% Predicate Set Management
%% =============================================

% Create a new predicate set
wasm_create_predicate_set(SetJson, Response) :-
    catch(
        (
            % Parse set from JSON
            atom_json_dict(SetJson, Set, []),

            % Extract set details
            get_dict(id, Set, Id),
            get_dict(name, Set, Name),
            get_dict_default(description, Set, "", Description),
            get_dict_default(predicates, Set, [], Predicates),

            % Convert to atoms
            atom_string(IdAtom, Id),
            atom_string(NameAtom, Name),
            atom_string(DescriptionAtom, Description),

            % Check if set already exists
            (predicate_set(IdAtom, _, _) ->
                json_error(409, 'Predicate set already exists', Response)
            ;
                % Create set
                assertz(predicate_set(IdAtom, NameAtom, DescriptionAtom)),

                % Add predicates to set
                maplist(add_predicate_to_set(IdAtom), Predicates),

                json_response(Set, Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Get a predicate set by ID
wasm_get_predicate_set(Id, Response) :-
    catch(
        (
            atom_string(IdAtom, Id),

            % Check if set exists
            (predicate_set(IdAtom, NameAtom, DescriptionAtom) ->
                % Get predicate members
                findall(
                    PredURI,
                    (
                        predicate_set_member(IdAtom, PredAtom),
                        atom_string(PredAtom, PredURI)
                    ),
                    Predicates
                ),

                % Convert to strings for JSON
                atom_string(NameAtom, Name),
                atom_string(DescriptionAtom, Description),

                % Create response
                SetDict = _{
                    id: Id,
                    name: Name,
                    description: Description,
                    predicates: Predicates
                },
                json_response(SetDict, Response)
            ;
                % Set not found
                json_error(404, 'Predicate set not found', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Update a predicate set
wasm_update_predicate_set(Id, UpdatesJson, Response) :-
    catch(
        (
            atom_string(IdAtom, Id),

            % Parse updates from JSON
            atom_json_dict(UpdatesJson, Updates, []),

            % Check if set exists
            (predicate_set(IdAtom, OldNameAtom, OldDescriptionAtom) ->
                % Get updated values (or use old ones if not specified)
                (get_dict(name, Updates, NewName) -> atom_string(NewNameAtom, NewName) ; NewNameAtom = OldNameAtom),
                (get_dict(description, Updates, NewDesc) -> atom_string(NewDescAtom, NewDesc) ; NewDescAtom = OldDescriptionAtom),

                % Update predicates if specified
                (get_dict(predicates, Updates, NewPredicates) ->
                    % Remove all existing predicates
                    retractall(predicate_set_member(IdAtom, _)),
                    % Add new predicates
                    maplist(add_predicate_to_set(IdAtom), NewPredicates)
                ; true),

                % Update set
                retractall(predicate_set(IdAtom, _, _)),
                assertz(predicate_set(IdAtom, NewNameAtom, NewDescAtom)),

                % Get current predicate members
                findall(
                    PredURI,
                    (
                        predicate_set_member(IdAtom, PredAtom),
                        atom_string(PredAtom, PredURI)
                    ),
                    CurrentPredicates
                ),

                % Convert to strings for JSON
                atom_string(NewNameAtom, Name),
                atom_string(NewDescAtom, Description),

                % Create response
                SetDict = _{
                    id: Id,
                    name: Name,
                    description: Description,
                    predicates: CurrentPredicates
                },
                json_response(SetDict, Response)
            ;
                % Set not found
                json_error(404, 'Predicate set not found', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Delete a predicate set
wasm_delete_predicate_set(Id, Response) :-
    catch(
        (
            atom_string(IdAtom, Id),

            % Check if set exists
            (predicate_set(IdAtom, _, _) ->
                % Delete set and members
                retractall(predicate_set(IdAtom, _, _)),
                retractall(predicate_set_member(IdAtom, _)),

                json_response(true, Response)
            ;
                % Set not found
                json_error(404, 'Predicate set not found', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Get all predicate sets
wasm_get_all_predicate_sets(Response) :-
    catch(
        (
            % Find all sets
            findall(
                _{
                    id: Id,
                    name: Name,
                    description: Description,
                    predicates: Predicates
                },
                (
                    predicate_set(IdAtom, NameAtom, DescriptionAtom),

                    % Get predicate members
                    findall(
                        PredURI,
                        (
                            predicate_set_member(IdAtom, PredAtom),
                            atom_string(PredAtom, PredURI)
                        ),
                        Predicates
                    ),

                    % Convert to strings for JSON
                    atom_string(IdAtom, Id),
                    atom_string(NameAtom, Name),
                    atom_string(DescriptionAtom, Description)
                ),
                Sets
            ),

            json_response(Sets, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Add predicates to a set
wasm_add_predicates_to_set(Id, PredicatesJson, Response) :-
    catch(
        (
            atom_string(IdAtom, Id),

            % Parse predicates from JSON
            atom_json_dict(PredicatesJson, Predicates, []),

            % Check if set exists
            (predicate_set(IdAtom, NameAtom, DescriptionAtom) ->
                % Add predicates to set
                maplist(add_predicate_to_set(IdAtom), Predicates),

                % Get updated predicate members
                findall(
                    PredURI,
                    (
                        predicate_set_member(IdAtom, PredAtom),
                        atom_string(PredAtom, PredURI)
                    ),
                    UpdatedPredicates
                ),

                % Convert to strings for JSON
                atom_string(NameAtom, Name),
                atom_string(DescriptionAtom, Description),

                % Create response
                SetDict = _{
                    id: Id,
                    name: Name,
                    description: Description,
                    predicates: UpdatedPredicates
                },
                json_response(SetDict, Response)
            ;
                % Set not found
                json_error(404, 'Predicate set not found', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Remove predicates from a set
wasm_remove_predicates_from_set(Id, PredicatesJson, Response) :-
    catch(
        (
            atom_string(IdAtom, Id),

            % Parse predicates from JSON
            atom_json_dict(PredicatesJson, Predicates, []),

            % Check if set exists
            (predicate_set(IdAtom, NameAtom, DescriptionAtom) ->
                % Remove predicates from set
                maplist(remove_predicate_from_set(IdAtom), Predicates),

                % Get updated predicate members
                findall(
                    PredURI,
                    (
                        predicate_set_member(IdAtom, PredAtom),
                        atom_string(PredAtom, PredURI)
                    ),
                    UpdatedPredicates
                ),

                % Convert to strings for JSON
                atom_string(NameAtom, Name),
                atom_string(DescriptionAtom, Description),

                % Create response
                SetDict = _{
                    id: Id,
                    name: Name,
                    description: Description,
                    predicates: UpdatedPredicates
                },
                json_response(SetDict, Response)
            ;
                % Set not found
                json_error(404, 'Predicate set not found', Response)
            )
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Helper to add a predicate to a set
add_predicate_to_set(SetId, PredicateURI) :-
    atom_string(PredicateAtom, PredicateURI),
    % Check if predicate exists
    (predicate_def(PredicateAtom, _, _, _, _, _, _) ->
        % Add to set if not already a member
        (predicate_set_member(SetId, PredicateAtom) ->
            true  % Already a member
        ;
            assertz(predicate_set_member(SetId, PredicateAtom))
        )
    ;
        % Predicate not found - skip
        true
    ).

% Helper to remove a predicate from a set
remove_predicate_from_set(SetId, PredicateURI) :-
    atom_string(PredicateAtom, PredicateURI),
    retractall(predicate_set_member(SetId, PredicateAtom)).

%% =============================================
%% Predicate Path Management
%% =============================================

% Create a new predicate path
wasm_create_predicate_path(PathJson, Response) :-
    catch(
        (
            % Parse path from JSON
            atom_json_dict(PathJson, PathDict, []),

            % Extract path details
            get_dict(path, PathDict, PathURIs),
            get_dict_default(label, PathDict, "", Label),
            get_dict(sourceType, PathDict, SourceType),
            get_dict(targetType, PathDict, TargetType),

            % Convert to atoms
            maplist(atom_string, PathAtoms, PathURIs),
            atom_string(LabelAtom, Label),
            atom_string(SourceTypeAtom, SourceType),
            atom_string(TargetTypeAtom, TargetType),

            % Generate path ID
            atomic_list_concat(PathAtoms, '.', PathIdAtom),

            % Store the path
            assertz(predicate_path(PathIdAtom, LabelAtom, SourceTypeAtom, TargetTypeAtom)),

            % Store each step in the path
            store_path_steps(PathIdAtom, PathAtoms),

            atom_string(PathIdAtom, PathId),
            PathDict2 = PathDict.put(id, PathId),

            json_response(PathDict2, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Parse a path expression into a predicate path
wasm_parse_path_expression(Expression, Response) :-
    catch(
        (
            atom_string(ExpressionAtom, Expression),

            % Use core path parsing
            parse_d_path(ExpressionAtom, Path),

            % Find source and target types
            (Path = [FirstPred|_] ->
                % Get source type from first predicate
                predicate_def(FirstPred, _, SourceType, _, _, _, _)
            ;
                % Empty path - use default
                SourceType = 'owl:Thing'
            ),

            (Path = [_|Rest], Rest = [_|_], last(Rest, LastPred) ->
                % Get target type from last predicate
                predicate_def(LastPred, _, _, TargetType, _, _, _)
            ; Path = [SinglePred] ->
                % Single predicate path
                predicate_def(SinglePred, _, _, TargetType, _, _, _)
            ;
                % Empty path - use default
                TargetType = 'owl:Thing'
            ),

            % Convert to strings for JSON
            maplist(atom_string, Path, PathURIs),
            atom_string(SourceType, SourceTypeStr),
            atom_string(TargetType, TargetTypeStr),

            % Create response
            PathDict = _{
                path: PathURIs,
                sourceType: SourceTypeStr,
                targetType: TargetTypeStr
            },

            json_response(PathDict, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Generate all possible paths between two entity types
wasm_generate_paths(SourceType, TargetType, MaxLength, Response) :-
    catch(
        (
            atom_string(SourceTypeAtom, SourceType),
            atom_string(TargetTypeAtom, TargetType),

            % Use core path generation
            generate_paths(SourceTypeAtom, TargetTypeAtom, MaxLength, Paths),

            % Convert to path dictionaries
            maplist(path_to_dict(SourceTypeAtom, TargetTypeAtom), Paths, PathDicts),

            json_response(PathDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Execute a predicate path query
wasm_execute_path_query(PathJson, ParamsJson, Response) :-
    catch(
        (
            % Parse path and params from JSON
            atom_json_dict(PathJson, PathDict, []),
            (ParamsJson \= null -> atom_json_dict(ParamsJson, Params, []) ; Params = _{}),

            % Extract path details
            get_dict(path, PathDict, PathURIs),
            get_dict(sourceType, PathDict, SourceType),
            get_dict(targetType, PathDict, TargetType),

            % Convert to atoms
            maplist(atom_string, PathAtoms, PathURIs),
            atom_string(SourceTypeAtom, SourceType),
            atom_string(TargetTypeAtom, TargetType),

            % Extract pagination parameters with defaults
            get_dict_default(limit, Params, 100, Limit),
            get_dict_default(offset, Params, 0, Offset),
            get_dict_default(minConfidence, Params, 0.0, MinConfidence),

            % Execute path query
            findall(
                (SubjectAtom, ObjectAtom, Confidence),
                (
                    % Find subjects of source type
                    rdf(SubjectAtom, rdf:type, SourceTypeAtom, _),

                    % Evaluate path
                    evaluate_path(PathAtoms, SubjectAtom, ObjectAtom, _),

                    % Verify object type
                    rdf(ObjectAtom, rdf:type, TargetTypeAtom, _),

                    % Calculate confidence (minimum of confidence scores along path)
                    path_confidence(SubjectAtom, ObjectAtom, PathAtoms, PathConfidence),
                    PathConfidence >= MinConfidence,
                    Confidence = PathConfidence
                ),
                AllResults
            ),

            % Count total results
            length(AllResults, TotalCount),

            % Apply pagination
            apply_pagination(AllResults, Offset, Limit, PagedTriples),

            % Convert to result dictionaries
            maplist(triple_to_result_dict, PagedTriples, ResultDicts),

            % Create paged results
            length(PagedTriples, ResultCount),
            HasMore = (ResultCount + Offset < TotalCount),
            ResultsDict = _{
                items: ResultDicts,
                totalCount: TotalCount,
                pageIndex: Offset // Limit,
                pageSize: Limit,
                hasMore: HasMore
            },

            json_response(ResultsDict, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Helper to store path steps
store_path_steps(_, []).
store_path_steps(PathId, [Pred|Rest]) :-
    % Store step
    assertz(path_step(PathId, Pred, Rest)),
    % Continue with rest of path
    store_path_steps(PathId, Rest).

% Helper to convert path to dictionary
path_to_dict(SourceType, TargetType, Path, Dict) :-
    maplist(atom_string, Path, PathURIs),
    atom_string(SourceType, SourceTypeStr),
    atom_string(TargetType, TargetTypeStr),
    Dict = _{
        path: PathURIs,
        sourceType: SourceTypeStr,
        targetType: TargetTypeStr
    }.

% Helper to get last element of a list
last([X], X) :- !.
last([_|Xs], Last) :- last(Xs, Last).

% Helper to calculate path confidence
path_confidence(Subject, Object, Path, Confidence) :-
    path_confidence(Subject, Object, Path, 1.0, Confidence).

path_confidence(_, _, [], Acc, Acc).
path_confidence(Subject, Object, [Pred|Rest], Acc, Confidence) :-
    % Find intermediate object
    (Rest = [] ->
        IntermediateObject = Object
    ;
        rdf(Subject, Pred, IntermediateObject, _)
    ),

    % Get confidence for this step
    (triple_confidence(Subject, Pred, IntermediateObject, _, StepConfidence) ->
        NewAcc is min(Acc, StepConfidence)
    ;
        NewAcc = Acc
    ),

    % Continue with rest of path
    path_confidence(IntermediateObject, Object, Rest, NewAcc, Confidence).

% Helper to convert triple to result dictionary
triple_to_result_dict((Subject, Object, Confidence), Dict) :-
    atom_string(Subject, SubjectStr),
    atom_string(Object, ObjectStr),
    Dict = _{
        subject: SubjectStr,
        object: ObjectStr,
        confidence: Confidence
    }.

%% =============================================
%% Gold Standard Evaluation
%% =============================================

% Define a gold standard fact
wasm_define_gold_fact(FactJson, Response) :-
    catch(
        (
            % Parse fact from JSON
            atom_json_dict(FactJson, Fact, []),

            % Extract fact details
            get_dict(predicate, Fact, Predicate),
            get_dict(entity, Fact, Entity),
            get_dict(truthValue, Fact, TruthValue),
            get_dict_default(confidence, Fact, 1.0, Confidence),

            % Convert to atoms
            atom_string(PredicateAtom, Predicate),
            atom_string(EntityAtom, Entity),

            % Store gold fact
            define_gold_fact(PredicateAtom, EntityAtom, TruthValue),

            % Store confidence if provided
            (Confidence < 1.0 ->
                assertz(gold_fact_confidence(PredicateAtom, EntityAtom, Confidence))
            ; true),

            json_response(Fact, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Define a logical constraint between predicates
wasm_define_logical_constraint(ConstraintJson, Response) :-
    catch(
        (
            % Parse constraint from JSON
            atom_json_dict(ConstraintJson, Constraint, []),

            % Extract constraint details
            get_dict(id, Constraint, Id),
            get_dict(sourcePredicateUri, Constraint, SourcePred),
            get_dict(targetPredicateUri, Constraint, TargetPred),
            get_dict(constraintType, Constraint, Type),
            get_dict_default(weight, Constraint, 1.0, Weight),

            % Convert to atoms
            atom_string(IdAtom, Id),
            atom_string(SourcePredAtom, SourcePred),
            atom_string(TargetPredAtom, TargetPred),
            atom_string(TypeAtom, Type),

            % Store constraint
            define_gold_constraint(SourcePredAtom, TargetPredAtom, TypeAtom),

            % Store weight if not default
            (Weight \= 1.0 ->
                assertz(constraint_weight(IdAtom, Weight))
            ; true),

            json_response(Constraint, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Create a constraint graph for gold standard evaluation
wasm_create_constraint_graph(GraphJson, Response) :-
    catch(
        (
            % Parse graph from JSON
            atom_json_dict(GraphJson, Graph, []),

            % Extract graph details
            get_dict(id, Graph, Id),
            get_dict(constraints, Graph, Constraints),
            get_dict_default(description, Graph, "", Description),

            % Convert to atoms
            atom_string(IdAtom, Id),
            atom_string(DescriptionAtom, Description),

            % Store graph
            (constraint_graph(IdAtom, _) ->
                retractall(constraint_graph(IdAtom, _)),
                retractall(constraint_graph_member(IdAtom, _))
            ; true),

            assertz(constraint_graph(IdAtom, DescriptionAtom)),

            % Store constraints
            maplist(add_constraint_to_graph(IdAtom), Constraints),

            json_response(Graph, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Evaluate a set of predicate instances against gold standard facts
wasm_evaluate_accuracy(InstancesJson, GraphId, Response) :-
    catch(
        (
            % Parse instances from JSON
            atom_json_dict(InstancesJson, Instances, []),
            atom_string(GraphIdAtom, GraphId),

            % Convert instances to predictions format
            instances_to_predictions(Instances, Predictions),

            % Get gold facts
            findall(gold(P, E, T), gold_fact(P, E, T), GoldFacts),

            % Evaluate accuracy using F1
            evaluate_accuracy(Predictions, GoldFacts, F1Score),

            % Get constraints for consistency measurement
            findall(constraint(S, T, Type),
                (constraint_graph_member(GraphIdAtom, ConstraintId),
                 gold_constraint(S, T, Type)),
                Constraints),

            % Convert instances to beliefs format
            instances_to_beliefs(Instances, Beliefs),

            % Measure consistency
            constraint_violation_metric(Beliefs, Constraints, ViolationRatio),
            ConsistencyScore is 1 - ViolationRatio,

            % Calculate combined confidence score
            % (weighted average of accuracy and consistency)
            ConfidenceScore is (F1Score + ConsistencyScore) / 2,

            % Create metrics response
            MetricsDict = _{
                accuracy: F1Score,
                consistency: ConsistencyScore,
                violationRatio: ViolationRatio,
                confidenceScore: ConfidenceScore
            },

            json_response(MetricsDict, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Measure the consistency of a set of predicate instances
wasm_measure_consistency(InstancesJson, GraphId, Response) :-
    catch(
        (
            % Parse instances from JSON
            atom_json_dict(InstancesJson, Instances, []),
            atom_string(GraphIdAtom, GraphId),

            % Get constraints for this graph
            findall(constraint(S, T, Type),
                (constraint_graph_member(GraphIdAtom, ConstraintId),
                 gold_constraint(S, T, Type)),
                Constraints),

            % Convert instances to beliefs format
            instances_to_beliefs(Instances, Beliefs),

            % Measure consistency
            constraint_violation_metric(Beliefs, Constraints, ViolationRatio),
            ConsistencyScore is 1 - ViolationRatio,

            % Create metrics response
            MetricsDict = _{
                consistency: ConsistencyScore,
                violationRatio: ViolationRatio,
                confidenceScore: ConsistencyScore
            },

            json_response(MetricsDict, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Helper to add constraint to graph
add_constraint_to_graph(GraphId, Constraint) :-
    get_dict(id, Constraint, Id),
    atom_string(IdAtom, Id),
    assertz(constraint_graph_member(GraphId, IdAtom)),

    % Add the constraint if not already defined
    get_dict(sourcePredicateUri, Constraint, SourcePred),
    get_dict(targetPredicateUri, Constraint, TargetPred),
    get_dict(constraintType, Constraint, Type),

    atom_string(SourcePredAtom, SourcePred),
    atom_string(TargetPredAtom, TargetPred),
    atom_string(TypeAtom, Type),

    (gold_constraint(SourcePredAtom, TargetPredAtom, _) ->
        % Already defined, update type
        retractall(gold_constraint(SourcePredAtom, TargetPredAtom, _)),
        assertz(gold_constraint(SourcePredAtom, TargetPredAtom, TypeAtom))
    ;
        % New constraint
        define_gold_constraint(SourcePredAtom, TargetPredAtom, TypeAtom)
    ),

    % Store weight if provided
    (get_dict(weight, Constraint, Weight), Weight \= 1.0 ->
        retractall(constraint_weight(IdAtom, _)),
        assertz(constraint_weight(IdAtom, Weight))
    ; true).

% Helper to convert instances to predictions format
instances_to_predictions(Instances, Predictions) :-
    findall(
        pred(PredAtom, EntityAtom, true),
        (
            member(Instance, Instances),
            get_dict(subject, Instance, Subject),
            get_dict(predicate, Instance, Predicate),
            get_dict(object, Instance, Object),
            % Each instance creates a prediction that the predicate holds for subject
            atom_string(PredAtom, Predicate),
            atom_string(EntityAtom, Subject)
        ),
        Predictions
    ).

% Helper to convert instances to beliefs format
instances_to_beliefs(Instances, Beliefs) :-
    findall(
        belief(EntityAtom, PredAtom, true),
        (
            member(Instance, Instances),
            get_dict(subject, Instance, Subject),
            get_dict(predicate, Instance, Predicate),
            atom_string(EntityAtom, Subject),
            atom_string(PredAtom, Predicate)
        ),
        Beliefs
    ).

%% =============================================
%% Predicate Discovery
%% =============================================

% Discover potential predicates between entity types
wasm_discover_potential_predicates(ParamsJson, Response) :-
    catch(
        (
            % Parse params from JSON
            atom_json_dict(ParamsJson, Params, []),

            % Extract params
            get_dict(sourceType, Params, SourceType),
            get_dict(targetType, Params, TargetType),
            get_dict_default(maxPathLength, Params, 3, MaxPathLength),
            get_dict_default(minFrequency, Params, 1, MinFrequency),
            get_dict_default(minConfidence, Params, 0.0, MinConfidence),

            % Convert to atoms
            atom_string(SourceTypeAtom, SourceType),
            atom_string(TargetTypeAtom, TargetType),

            % Discover direct predicates
            discover_potential_predicates(SourceTypeAtom, TargetTypeAtom, DirectPredicates),

            % Convert to predicate dictionaries
            findall(
                PredDict,
                (
                    member(PredAtom, DirectPredicates),
                    predicate_def(PredAtom, Label, Domain, Range, Inverse, Transitive, Symmetric),

                    % Convert to strings for JSON
                    atom_string(PredAtom, PredURI),
                    atom_string(Label, LabelStr),
                    atom_string(Domain, DomainStr),
                    atom_string(Range, RangeStr),
                    (Inverse \= null -> atom_string(Inverse, InverseStr) ; InverseStr = null),

                    PredDict = _{
                        uri: PredURI,
                        label: LabelStr,
                        domain: DomainStr,
                        range: RangeStr,
                        inverse: InverseStr,
                        transitive: Transitive,
                        symmetric: Symmetric
                    }
                ),
                PredicateDicts
            ),

            json_response(PredicateDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Discover common predicate path patterns
wasm_discover_path_patterns(MaxLength, MinFrequency, Response) :-
    catch(
        (
            % Discover patterns
            discover_path_patterns(MaxLength, MinFrequency, PathPatterns),

            % Convert to JSON-friendly format
            findall(
                _{
                    path: PathStr,
                    frequency: Count
                },
                (
                    member(Path-Count, PathPatterns),
                    path_to_string(Path, PathStr)
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Find missing predicates based on constraints
wasm_find_missing_predicates(GraphId, Response) :-
    catch(
        (
            atom_string(GraphIdAtom, GraphId),

            % Get all entity types
            findall(
                TypeAtom,
                (
                    rdf(_, rdf:type, TypeAtom, _),
                    TypeAtom \= rdf:type
                ),
                Types
            ),

            % Find missing predicates for all type pairs
            findall(
                _{
                    subject: SubjectStr,
                    predicate: PredStr,
                    object: ObjectStr
                },
                (
                    % For each pair of types
                    member(SourceType, Types),
                    member(TargetType, Types),

                    % Find missing predicates
                    find_missing_predicates(SourceType, TargetType, MissingPreds),

                    % Extract details
                    member(missing(Subject, Object, Pred), MissingPreds),

                    % Convert to strings for JSON
                    atom_string(Subject, SubjectStr),
                    atom_string(Pred, PredStr),
                    atom_string(Object, ObjectStr)
                ),
                MissingPredicates
            ),

            json_response(MissingPredicates, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Suggest new constraint candidates based on observed patterns
wasm_suggest_constraint_candidates(ConfidenceThreshold, Response) :-
    catch(
        (
            % Suggest candidates
            suggest_constraint_candidates(Suggestions),

            % Filter by confidence threshold
            findall(
                _{
                    source: SourceStr,
                    target: TargetStr,
                    confidence: Confidence
                },
                (
                    member(SourcePred-TargetPred-Confidence, Suggestions),
                    Confidence >= ConfidenceThreshold,

                    % Convert to strings for JSON
                    atom_string(SourcePred, SourceStr),
                    atom_string(TargetPred, TargetStr)
                ),
                Candidates
            ),

            json_response(Candidates, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Complete missing predicate instances based on constraints
wasm_complete_missing_predicates(GraphId, ConfidenceThreshold, Response) :-
    catch(
        (
            atom_string(GraphIdAtom, GraphId),

            % Get all entity types
            findall(
                TypeAtom,
                (
                    rdf(_, rdf:type, TypeAtom, _),
                    TypeAtom \= rdf:type
                ),
                Types
            ),

            % Complete missing paths for all type pairs
            findall(
                _{
                    subject: SubjectStr,
                    predicate: PredStr,
                    object: ObjectStr,
                    confidence: Confidence
                },
                (
                    % For each pair of types
                    member(SourceType, Types),
                    member(TargetType, Types),

                    % Complete missing paths
                    complete_missing_paths(SourceType, TargetType, ConfidenceThreshold, CompletedPaths),

                    % Extract details
                    member(Subject-Object-Path-Confidence, CompletedPaths),

                    % For each missing path, suggest a direct predicate
                    % (simplified approach - could be more sophisticated)
                    path_to_string(Path, PathStr),

                    % Use path as predicate name
                    PredStr = PathStr,

                    % Convert entities to strings for JSON
                    atom_string(Subject, SubjectStr),
                    atom_string(Object, ObjectStr)
                ),
                CompletedPredicates
            ),

            json_response(CompletedPredicates, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Helper to convert a path to a string
path_to_string(Path, String) :-
    maplist(atom_string, Path, PathStrings),
    atomic_list_concat(PathStrings, '.', String).

%% Enhanced Predicate Discovery Module for RDF Knowledge Engine
%% Extends the category theory approach with gold standard evaluation
%% Date: March 14, 2025

:- module(cat_rdf_discovery, [
    % Core discovery predicates
    discover_entities/2,
    discover_entity_patterns/2,
    discover_predicate_correlations/2,
    discover_type_hierarchies/1,
    infer_missing_predicates/3,

    % Gold standard guided discovery
    discover_with_gold_guidance/3,
    rank_discoveries_by_constraint_satisfaction/3,
    identify_constraint_violations/2,
    suggest_corrections/2,

    % Path-based discovery
    discover_path_equivalences/2,
    extract_path_patterns/3,
    compress_paths/2,
    generalize_paths/3,

    % WASM interfaces
    wasm_discover_entities/1,
    wasm_discover_entity_patterns/1,
    wasm_discover_predicate_correlations/1,
    wasm_discover_type_hierarchies/0,
    wasm_infer_missing_predicates/2,
    wasm_discover_with_gold_guidance/2,
    wasm_discover_path_equivalences/1,
    wasm_extract_path_patterns/2,
    wasm_suggest_corrections/1
]).

:- use_module(library(http/json)).
:- use_module(cat_rdf).
:- use_module(cat_rdf_predicate).

%% =============================================
%% Core Discovery Predicates
%% =============================================

% Discover entities with specific patterns of predicates
discover_entities(PatternPredicates, Entities) :-
    % Find entities that have all the specified predicates
    findall(Entity, (
        % Get all entities
        rdf(Entity, _, _, _),
        % Ensure it's a proper entity (not a literal)
        \+ rdf_is_literal(Entity),
        % Check that it has all predicates
        forall(member(Pred, PatternPredicates), (
            rdf(Entity, Pred, _, _)
        ))
    ), Entities).

% Discover patterns of predicates that commonly co-occur on entities
discover_entity_patterns(MinSupport, Patterns) :-
    % Find all predicates
    findall(Pred, (
        rdf(_, Pred, _, _),
        % Exclude system predicates
        \+ (Pred = rdf:type)
    ), AllPredicates),

    % Remove duplicates
    sort(AllPredicates, UniquePredicates),

    % Generate all possible predicate combinations (up to a reasonable size)
    findall(Pattern, (
        % Get subsets of size 2-4 predicates
        between(2, 4, Size),
        findall(P, (
            length(Pattern, Size),
            subset(Pattern, UniquePredicates)
        ), AllPatterns),
        member(Pattern, AllPatterns)
    ), CandidatePatterns),

    % Count entities that match each pattern
    findall(Pattern-Count, (
        member(Pattern, CandidatePatterns),
        findall(E, (
            rdf(E, rdf:type, _, _),  % Get all typed entities
            forall(member(P, Pattern), rdf(E, P, _, _))
        ), MatchingEntities),
        length(MatchingEntities, Count),
        Count >= MinSupport
    ), PatternCounts),

    % Sort by count
    sort(2, @>=, PatternCounts, SortedPatterns),

    % Extract patterns
    findall(pattern(P, C), member(P-C, SortedPatterns), Patterns).

% Discover correlations between predicates
discover_predicate_correlations(MinCorrelation, Correlations) :-
    % Find all predicates
    findall(Pred, (
        rdf(_, Pred, _, _),
        % Exclude system predicates
        \+ (Pred = rdf:type)
    ), AllPredicates),

    % Remove duplicates
    sort(AllPredicates, UniquePredicates),

    % Calculate correlation for each predicate pair
    findall(Pred1-Pred2-Correlation, (
        member(Pred1, UniquePredicates),
        member(Pred2, UniquePredicates),
        Pred1 @< Pred2,  % Avoid duplicate pairs

        % Count entities with Pred1
        findall(E1, rdf(E1, Pred1, _, _), EntitiesWithPred1),
        sort(EntitiesWithPred1, UniqueEntitiesWithPred1),
        length(UniqueEntitiesWithPred1, CountPred1),

        % Count entities with Pred2
        findall(E2, rdf(E2, Pred2, _, _), EntitiesWithPred2),
        sort(EntitiesWithPred2, UniqueEntitiesWithPred2),
        length(UniqueEntitiesWithPred2, CountPred2),

        % Count entities with both predicates
        intersection(UniqueEntitiesWithPred1, UniqueEntitiesWithPred2, EntitiesWithBoth),
        length(EntitiesWithBoth, CountBoth),

        % Calculate correlation (Jaccard index)
        (CountPred1 + CountPred2 - CountBoth) > 0,
        Correlation is CountBoth / (CountPred1 + CountPred2 - CountBoth),
        Correlation >= MinCorrelation
    ), Correlations).

% Discover type hierarchies in the knowledge base
discover_type_hierarchies(Hierarchies) :-
    % Find all entity types
    findall(Type, (
        rdf(_, rdf:type, Type, _),
        Type \= rdf:type
    ), AllTypes),

    % Remove duplicates
    sort(AllTypes, UniqueTypes),

    % Find hierarchical relationships
    findall(parent(ParentType, ChildType), (
        member(ParentType, UniqueTypes),
        member(ChildType, UniqueTypes),
        ParentType \= ChildType,

        % Check if all entities of ChildType are also of ParentType
        findall(E, rdf(E, rdf:type, ChildType, _), ChildEntities),
        ChildEntities \= [],  % Ensure non-empty
        forall(member(E, ChildEntities), rdf(E, rdf:type, ParentType, _))
    ), ParentChildPairs),

    % Build hierarchical tree
    build_hierarchy_tree(ParentChildPairs, UniqueTypes, Hierarchies).

% Infer missing predicates based on entity similarities
infer_missing_predicates(Entity, SimilarityThreshold, MissingPredicates) :-
    % Find similar entities
    find_similar_entities(Entity, SimilarityThreshold, SimilarEntities),

    % Find predicates that similar entities have but this entity doesn't
    findall(missing(Pred, Object), (
        member(SimilarEntity, SimilarEntities),
        rdf(SimilarEntity, Pred, Object, _),
        \+ rdf(Entity, Pred, _, _),
        % Exclude system predicates
        Pred \= rdf:type
    ), MissingPredicates).

%% =============================================
%% Gold Standard Guided Discovery
%% =============================================

% Use gold standard constraints to guide discovery
discover_with_gold_guidance(EntityType, ConstraintGraphId, Discoveries) :-
    % Get entities of the specified type
    findall(Entity, rdf(Entity, rdf:type, EntityType, _), Entities),

    % Get constraints for this graph
    findall(constraint(S, T, Type), (
        constraint_graph_member(ConstraintGraphId, ConstraintId),
        gold_constraint(S, T, Type)
    ), Constraints),

    % Find potential discoveries for each entity
    findall(discovery(Entity, Pred, Object, Confidence), (
        member(Entity, Entities),
        member(constraint(SourcePred, TargetPred, implication), Constraints),

        % If entity has source predicate but not target, suggest target
        rdf(Entity, SourcePred, SourceObject, _),
        \+ rdf(Entity, TargetPred, _, _),

        % Predict object and confidence
        predict_object_for_predicate(Entity, TargetPred, SourceObject, Object, Confidence),
        Confidence > 0.5  % Only include reasonably confident predictions
    ), Discoveries).

% Rank discoveries by how well they satisfy constraints
rank_discoveries_by_constraint_satisfaction(Discoveries, Constraints, RankedDiscoveries) :-
    % Score each discovery
    findall(Discovery-Score, (
        member(Discovery, Discoveries),
        Discovery = discovery(Entity, Pred, Object, _),

        % Calculate how many constraints would be satisfied
        findall(1, (
            member(constraint(SourcePred, TargetPred, implication), Constraints),
            ((Pred = TargetPred, rdf(Entity, SourcePred, _, _)) ;
             (Pred = SourcePred, rdf(Entity, TargetPred, _, _)))
        ), Satisfactions),
        length(Satisfactions, Score)
    ), ScoredDiscoveries),

    % Sort by score
    sort(2, @>=, ScoredDiscoveries, RankedDiscoveries).

% Identify constraint violations in the knowledge base
identify_constraint_violations(ConstraintGraphId, Violations) :-
    % Get constraints for this graph
    findall(constraint(S, T, Type), (
        constraint_graph_member(ConstraintGraphId, ConstraintId),
        gold_constraint(S, T, Type)
    ), Constraints),

    % Find violations for each constraint
    findall(violation(Entity, SourcePred, TargetPred), (
        member(constraint(SourcePred, TargetPred, implication), Constraints),
        rdf(Entity, SourcePred, _, _),
        \+ rdf(Entity, TargetPred, _, _)
    ), Violations).

% Suggest corrections for constraint violations
suggest_corrections(Violations, Corrections) :-
    % Generate correction for each violation
    findall(correction(Entity, Pred, Object, Confidence), (
        member(violation(Entity, SourcePred, TargetPred), Violations),
        Pred = TargetPred,

        % Try to predict object for the missing predicate
        predict_object_for_predicate(Entity, TargetPred, _, Object, Confidence),
        Confidence > 0.5  % Only include reasonably confident corrections
    ), Corrections).

%% =============================================
%% Path-based Discovery
%% =============================================

% Discover equivalent paths (paths that connect the same entities)
discover_path_equivalences(MinEquivalence, Equivalences) :-
    % Find all paths of length 1-3
    findall(Path, (
        between(1, 3, Length),
        find_paths_of_length(Length, Path)
    ), Paths),

    % Compare paths for equivalence
    findall(Path1-Path2-Score, (
        member(Path1, Paths),
        member(Path2, Paths),
        Path1 @< Path2,  % Avoid duplicates

        % Find entities connected by Path1
        findall(Start-End, path_connects(Start, End, Path1), Connections1),
        length(Connections1, Count1),
        Count1 > 0,

        % Find entities connected by Path2
        findall(Start-End, path_connects(Start, End, Path2), Connections2),
        length(Connections2, Count2),
        Count2 > 0,

        % Count common connections
        intersection(Connections1, Connections2, Common),
        length(Common, CommonCount),

        % Calculate equivalence score
        Score is CommonCount / max(Count1, Count2),
        Score >= MinEquivalence
    ), Equivalences).

% Extract common path patterns that occur in the knowledge base
extract_path_patterns(MaxLength, MinOccurrences, Patterns) :-
    % Find all paths up to MaxLength
    findall(Path, (
        between(1, MaxLength, Length),
        find_paths_of_length(Length, Path)
    ), AllPaths),

    % Count occurrences of each path
    count_path_occurrences(AllPaths, PathCounts),

    % Filter by minimum occurrences
    findall(Pattern-Count, (
        member(Path-Count, PathCounts),
        Count >= MinOccurrences,
        path_to_pattern(Path, Pattern)
    ), Patterns).

% Compress paths by removing redundant predicates
compress_paths(Paths, CompressedPaths) :-
    maplist(compress_path, Paths, CompressedPaths).

% Generalize paths by finding common patterns
generalize_paths(Paths, MinSupport, GeneralizedPatterns) :-
    % Extract predicate sequences from paths
    maplist(extract_predicates, Paths, PredicateSequences),

    % Find common subsequences
    find_common_subsequences(PredicateSequences, MinSupport, CommonSubsequences),

    % Convert to pattern form
    maplist(subsequence_to_pattern, CommonSubsequences, GeneralizedPatterns).

%% =============================================
%% Helper Predicates
%% =============================================

% Build a hierarchical tree from parent-child relationships
build_hierarchy_tree(ParentChildPairs, AllTypes, Hierarchies) :-
    % Find root types (those that are not children in any pair)
    findall(Root, (
        member(Root, AllTypes),
        \+ (member(parent(_, Root), ParentChildPairs))
    ), RootTypes),

    % Build tree for each root
    maplist(build_subtree(ParentChildPairs), RootTypes, Hierarchies).

% Build a subtree for a given root
build_subtree(ParentChildPairs, Root, tree(Root, Children)) :-
    % Find direct children of this root
    findall(Child, member(parent(Root, Child), ParentChildPairs), DirectChildren),

    % Recursively build subtrees for each child
    maplist(build_subtree(ParentChildPairs), DirectChildren, Children).

% Find similar entities based on predicate overlap
find_similar_entities(Entity, SimilarityThreshold, SimilarEntities) :-
    % Get all predicates for the entity
    findall(Pred, rdf(Entity, Pred, _, _), EntityPredicates),

    % Find other entities and calculate similarity
    findall(OtherEntity-Similarity, (
        rdf(OtherEntity, rdf:type, _, _),
        OtherEntity \= Entity,

        % Get predicates for other entity
        findall(OtherPred, rdf(OtherEntity, OtherPred, _, _), OtherPredicates),

        % Calculate Jaccard similarity
        intersection(EntityPredicates, OtherPredicates, CommonPredicates),
        union(EntityPredicates, OtherPredicates, AllPredicates),
        length(CommonPredicates, CommonCount),
        length(AllPredicates, TotalCount),
        Similarity is CommonCount / TotalCount,
        Similarity >= SimilarityThreshold
    ), SimilarityPairs),

    % Extract entities and sort by similarity
    sort(2, @>=, SimilarityPairs, SortedPairs),
    findall(E, member(E-_, SortedPairs), SimilarEntities).

% Predict an object for a given entity and predicate
predict_object_for_predicate(Entity, Predicate, SourceObject, Object, Confidence) :-
    % Strategy 1: Check for path-based relationship
    (path_based_prediction(Entity, Predicate, SourceObject, Object, Confidence) -> true ;

    % Strategy 2: Check similar entities
    (similar_entity_prediction(Entity, Predicate, Object, Confidence) -> true ;

    % Strategy 3: Use statistical correlation
    statistical_prediction(Entity, Predicate, Object, Confidence))).

% Predict object based on path relationships
path_based_prediction(Entity, Predicate, SourceObject, Object, Confidence) :-
    % Find paths that could lead to the predicate
    findall(Path-EndObject, (
        % Look for paths up to length 3
        between(1, 3, Length),
        path_of_length(Entity, EndObject, Path, Length),
        % Check if path has high overlap with predicate
        path_predicate_correlation(Path, Predicate, CorrelationScore),
        CorrelationScore > 0.7
    ), PathObjects),

    % If we found any candidates, use the one with highest correlation
    (PathObjects = [Path-Object|_] ->
        % Path correlation gives us confidence
        path_predicate_correlation(Path, Predicate, Confidence)
    ;
        % No path found
        fail
    ).

% Predict object based on similar entities
similar_entity_prediction(Entity, Predicate, Object, Confidence) :-
    % Find similar entities that have this predicate
    find_similar_entities(Entity, 0.6, SimilarEntities),

    % Find objects for similar entities
    findall(SimilarObject-Similarity, (
        member(SimilarEntity, SimilarEntities),
        rdf(SimilarEntity, Predicate, SimilarObject, _),
        entity_similarity(Entity, SimilarEntity, Similarity)
    ), ObjectSimilarities),

    % Group by object and aggregate similarities
    group_by_object(ObjectSimilarities, GroupedObjects),

    % Select object with highest aggregated similarity
    (GroupedObjects = [Object-Score|_] ->
        % Normalize score to get confidence
        Confidence is min(1.0, Score / 2.0)
    ;
        % No objects found
        fail
    ).

% Predict object based on statistical correlations
statistical_prediction(Entity, Predicate, Object, Confidence) :-
    % Find correlated predicates
    findall(CorrelatedPred-Correlation, (
        rdf(Entity, OtherPred, OtherObject, _),
        predicate_correlation(OtherPred, Predicate, Correlation),
        Correlation > 0.5
    ), CorrelatedPreds),

    % Find candidate objects based on predicate co-occurrence
    findall(CandidateObject-ObjectScore, (
        member(CorrelatedPred-Correlation, CorrelatedPreds),
        rdf(Entity, CorrelatedPred, IntermediateObject, _),
        object_transition_probability(IntermediateObject, Predicate, CandidateObject, TransitionProb),
        ObjectScore is Correlation * TransitionProb
    ), CandidateScores),

    % Group by object and sum scores
    group_by_object(CandidateScores, GroupedCandidates),

    % Select object with highest score
    (GroupedCandidates = [Object-Score|_] ->
        % Normalize score to get confidence
        Confidence is min(1.0, Score)
    ;
        % No objects found
        fail
    ).

% Find all paths of a given length
find_paths_of_length(Length, Path) :-
    % Find a valid path in the RDF data
    rdf(Start, FirstPred, Mid1, _),
    (Length = 1 ->
        Path = [FirstPred],
        Mid1 = End
    ; Length = 2 ->
        rdf(Mid1, SecondPred, End, _),
        Path = [FirstPred, SecondPred]
    ; Length = 3 ->
        rdf(Mid1, SecondPred, Mid2, _),
        rdf(Mid2, ThirdPred, End, _),
        Path = [FirstPred, SecondPred, ThirdPred]
    ).

% Check if a path connects two entities
path_connects(Start, End, Path) :-
    path_connects(Start, End, Path, []).

path_connects(Entity, Entity, [], _).
path_connects(Start, End, [Pred|Rest], Visited) :-
    rdf(Start, Pred, Mid, _),
    \+ member(Mid, Visited),
    path_connects(Mid, End, Rest, [Mid|Visited]).

% Count occurrences of each path
count_path_occurrences(Paths, PathCounts) :-
    % Count how many entity pairs each path connects
    findall(Path-Count, (
        member(Path, Paths),
        findall(Start-End, path_connects(Start, End, Path), Connections),
        length(Connections, Count)
    ), PathCounts).

% Convert a path to a pattern
path_to_pattern(Path, Pattern) :-
    % Extract domain/range information for each predicate
    maplist(predicate_to_pattern, Path, Pattern).

predicate_to_pattern(Pred, pattern(Pred, Domain, Range)) :-
    % Get domain and range from predicate definition if available
    (predicate_def(Pred, _, Domain, Range, _, _, _) ->
        true
    ;
        % Default to generic types
        Domain = 'owl:Thing',
        Range = 'owl:Thing'
    ).

% Compress a path by removing redundant predicates
compress_path(Path, Compressed) :-
    % Check if there are equivalent shorter paths
    (find_equivalent_shorter_path(Path, ShorterPath) ->
        Compressed = ShorterPath
    ;
        Compressed = Path
    ).

% Find an equivalent shorter path if possible
find_equivalent_shorter_path(Path, ShorterPath) :-
    % Try all prefixes + suffixes
    append(Prefix, Suffix, Path),
    Prefix \= [],
    Suffix \= [],

    % Find start and end entities for original path
    rdf(Start, _, _, _),
    path_connects(Start, End, Path),

    % Check if shorter path connects the same entities
    append(PrefixSuffix, _, Suffix),
    PrefixSuffix \= Suffix,
    append(Prefix, PrefixSuffix, ShorterPath),
    path_connects(Start, End, ShorterPath).

% Extract predicates from a path
extract_predicates(Path, Predicates) :-
    % Just extract the predicate URIs
    Predicates = Path.

% Find common subsequences in a list of sequences
find_common_subsequences(Sequences, MinSupport, CommonSubsequences) :-
    % Find all subsequences
    findall(Subseq-Count, (
        member(Seq, Sequences),
        subseq(Subseq, Seq),
        length(Subseq, Len),
        Len >= 2,  % Only consider meaningful subsequences
        count_occurrences(Subseq, Sequences, Count),
        Count >= MinSupport
    ), AllSubseqs),

    % Sort by count
    sort(2, @>=, AllSubseqs, SortedSubseqs),

    % Remove redundant subsequences
    remove_redundant(SortedSubseqs, CommonSubsequences).

% Count occurrences of a subsequence
count_occurrences(Subseq, Sequences, Count) :-
    findall(1, (
        member(Seq, Sequences),
        subseq(Subseq, Seq)
    ), Matches),
    length(Matches, Count).

% Convert a subsequence to a pattern
subsequence_to_pattern(Subseq-Count, pattern(Subseq, Count)).

% Check the correlation between a path and a predicate
path_predicate_correlation(Path, Predicate, Correlation) :-
    % Find all entity pairs connected by the path
    findall(Start-End, path_connects(Start, End, Path), PathConnections),
    length(PathConnections, PathCount),

    % Find all entity pairs connected by the predicate
    findall(S-O, rdf(S, Predicate, O, _), PredConnections),
    length(PredConnections, PredCount),

    % Find overlap
    intersection(PathConnections, PredConnections, Overlap),
    length(Overlap, OverlapCount),

    % Calculate Jaccard similarity
    (PathCount + PredCount - OverlapCount) > 0,
    Correlation is OverlapCount / (PathCount + PredCount - OverlapCount).

% Calculate similarity between entities
entity_similarity(Entity1, Entity2, Similarity) :-
    % Get predicates for both entities
    findall(Pred, rdf(Entity1, Pred, _, _), Preds1),
    findall(Pred, rdf(Entity2, Pred, _, _), Preds2),

    % Calculate Jaccard similarity
    intersection(Preds1, Preds2, Common),
    union(Preds1, Preds2, All),
    length(Common, CommonCount),
    length(All, AllCount),
    Similarity is CommonCount / AllCount.

% Calculate correlation between predicates
predicate_correlation(Pred1, Pred2, Correlation) :-
    % Find entities with each predicate
    findall(E1, rdf(E1, Pred1, _, _), Entities1),
    findall(E2, rdf(E2, Pred2, _, _), Entities2),

    % Calculate Jaccard similarity
    sort(Entities1, SortedEntities1),
    sort(Entities2, SortedEntities2),
    intersection(SortedEntities1, SortedEntities2, Common),
    union(SortedEntities1, SortedEntities2, All),
    length(Common, CommonCount),
    length(All, AllCount),
    AllCount > 0,
    Correlation is CommonCount / AllCount.

% Calculate probability of transition between objects
object_transition_probability(SourceObject, Predicate, TargetObject, Probability) :-
    % Find all entities that relate to SourceObject
    findall(E, (
        rdf(E, _, SourceObject, _)
    ), RelatedEntities),

    % Count how many of those also relate to TargetObject through Predicate
    findall(1, (
        member(E, RelatedEntities),
        rdf(E, Predicate, TargetObject, _)
    ), Transitions),

    length(RelatedEntities, TotalCount),
    length(Transitions, TransitionCount),
    TotalCount > 0,
    Probability is TransitionCount / TotalCount.

% Group objects and sum scores
group_by_object(ObjectScores, Grouped) :-
    % Sort by object
    sort(1, @=<, ObjectScores, Sorted),

    % Group and sum
    group_and_sum(Sorted, [], Grouped),

    % Sort by score
    sort(2, @>=, Grouped, Grouped).

% Group objects and sum their scores
group_and_sum([], Acc, Sorted) :-
    sort(2, @>=, Acc, Sorted).
group_and_sum([Object-Score|Rest], Acc, Result) :-
    % Check if object is already in accumulator
    (select(Object-AccScore, Acc, AccRest) ->
        % Update score
        NewScore is AccScore + Score,
        NewAcc = [Object-NewScore|AccRest]
    ;
        % Add new object
        NewAcc = [Object-Score|Acc]
    ),
    group_and_sum(Rest, NewAcc, Result).

% Check if a sequence is a subsequence of another
subseq([], _).
subseq([X|Xs], [X|Ys]) :- subseq(Xs, Ys).
subseq([X|Xs], [_|Ys]) :- subseq([X|Xs], Ys).

% Remove redundant subsequences (those contained in others with similar support)
remove_redundant([], []).
remove_redundant([Subseq-Count|Rest], [Subseq-Count|Filtered]) :-
    % Remove all subsequences of this one with similar count
    filter_out_contained(Subseq, Count, Rest, Remaining),
    remove_redundant(Remaining, Filtered).

% Filter out subsequences contained in a larger one with similar support
filter_out_contained(_, _, [], []).
filter_out_contained(LargeSubseq, LargeCount, [Subseq-Count|Rest], Filtered) :-
    Subseq = SubseqSeq-_,
    LargeSubseq = LargeSubseqSeq-_,
    (subseq(SubseqSeq, LargeSubseqSeq), Count =< LargeCount * 1.1 ->
        % This is redundant, skip it
        filter_out_contained(LargeSubseq, LargeCount, Rest, Filtered)
    ;
        % Keep this one
        Filtered = [Subseq-Count|Remaining],
        filter_out_contained(LargeSubseq, LargeCount, Rest, Remaining)
    ).

%% =============================================
%% WASM Interface Predicates
%% =============================================

% WASM interface for discover_entities
wasm_discover_entities(ParamsJson, Response) :-
    catch(
        (
            % Parse params from JSON
            atom_json_dict(ParamsJson, Params, []),

            % Extract predicates
            get_dict(predicates, Params, PredicateURIs),

            % Convert to atoms
            maplist(atom_string, PredicateAtoms, PredicateURIs),

            % Call discovery
            discover_entities(PredicateAtoms, EntityAtoms),

            % Convert to strings for JSON
            maplist(atom_string, EntityAtoms, EntityURIs),

            json_response(EntityURIs, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for discover_entity_patterns
wasm_discover_entity_patterns(ParamsJson, Response) :-
    catch(
        (
            % Parse params from JSON
            atom_json_dict(ParamsJson, Params, []),

            % Extract min support
            get_dict(minSupport, Params, MinSupport),

            % Call discovery
            discover_entity_patterns(MinSupport, Patterns),

            % Convert to JSON format
            findall(
                _{
                    predicates: PatternURIs,
                    support: Count
                },
                (
                    member(pattern(Pattern, Count), Patterns),
                    maplist(atom_string, Pattern, PatternURIs)
                ),
                PatternDicts
            ),

            json_response(PatternDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for discover_predicate_correlations
wasm_discover_predicate_correlations(ParamsJson, Response) :-
    catch(
        (
            % Parse params from JSON
            atom_json_dict(ParamsJson, Params, []),

            % Extract min correlation
            get_dict(minCorrelation, Params, MinCorrelation),

            % Call discovery
            discover_predicate_correlations(MinCorrelation, Correlations),

            % Convert to JSON format
            findall(
                _{
                    source: SourceStr,
                    target: TargetStr,
                    correlation: Correlation
                },
                (
                    member(Source-Target-Correlation, Correlations),
                    atom_string(Source, SourceStr),
                    atom_string(Target, TargetStr)
                ),
                CorrelationDicts
            ),

            json_response(CorrelationDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for discover_type_hierarchies
wasm_discover_type_hierarchies(Response) :-
    catch(
        (
            % Call discovery
            discover_type_hierarchies(Hierarchies),

            % Convert to JSON format
            maplist(hierarchy_tree_to_dict, Hierarchies, HierarchyDicts),

            json_response(HierarchyDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for infer_missing_predicates
wasm_infer_missing_predicates(EntityURI, ParamsJson, Response) :-
    catch(
        (
            % Parse params from JSON
            atom_json_dict(ParamsJson, Params, []),

            % Extract threshold
            get_dict(similarityThreshold, Params, SimilarityThreshold),

            % Convert to atom
            atom_string(EntityAtom, EntityURI),

            % Call inference
            infer_missing_predicates(EntityAtom, SimilarityThreshold, MissingPredicates),

            % Convert to JSON format
            findall(
                _{
                    predicate: PredStr,
                    object: ObjStr
                },
                (
                    member(missing(Pred, Obj), MissingPredicates),
                    atom_string(Pred, PredStr),
                    atom_string(Obj, ObjStr)
                ),
                MissingDicts
            ),

            json_response(MissingDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for discover_with_gold_guidance
wasm_discover_with_gold_guidance(TypeURI, GraphId, Response) :-
    catch(
        (
            % Convert to atoms
            atom_string(TypeAtom, TypeURI),
            atom_string(GraphIdAtom, GraphId),

            % Call discovery
            discover_with_gold_guidance(TypeAtom, GraphIdAtom, Discoveries),

            % Convert to JSON format
            findall(
                _{
                    entity: EntityStr,
                    predicate: PredStr,
                    object: ObjStr,
                    confidence: Confidence
                },
                (
                    member(discovery(Entity, Pred, Obj, Confidence), Discoveries),
                    atom_string(Entity, EntityStr),
                    atom_string(Pred, PredStr),
                    atom_string(Obj, ObjStr)
                ),
                DiscoveryDicts
            ),

            json_response(DiscoveryDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for discover_path_equivalences
wasm_discover_path_equivalences(ParamsJson, Response) :-
    catch(
        (
            % Parse params from JSON
            atom_json_dict(ParamsJson, Params, []),

            % Extract min equivalence
            get_dict(minEquivalence, Params, MinEquivalence),

            % Call discovery
            discover_path_equivalences(MinEquivalence, Equivalences),

            % Convert to JSON format
            findall(
                _{
                    path1: PathStr1,
                    path2: PathStr2,
                    equivalenceScore: Score
                },
                (
                    member(Path1-Path2-Score, Equivalences),
                    path_to_string(Path1, PathStr1),
                    path_to_string(Path2, PathStr2)
                ),
                EquivalenceDicts
            ),

            json_response(EquivalenceDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for extract_path_patterns
wasm_extract_path_patterns(MaxLength, ParamsJson, Response) :-
    catch(
        (
            % Parse params from JSON
            atom_json_dict(ParamsJson, Params, []),

            % Extract min occurrences
            get_dict(minOccurrences, Params, MinOccurrences),

            % Call discovery
            extract_path_patterns(MaxLength, MinOccurrences, Patterns),

            % Convert to JSON format
            findall(
                _{
                    pattern: PatternStr,
                    occurrences: Count
                },
                (
                    member(Pattern-Count, Patterns),
                    pattern_to_string(Pattern, PatternStr)
                ),
                PatternDicts
            ),

            json_response(PatternDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for suggest_corrections
wasm_suggest_corrections(GraphId, Response) :-
    catch(
        (
            % Convert to atom
            atom_string(GraphIdAtom, GraphId),

            % Find violations
            identify_constraint_violations(GraphIdAtom, Violations),

            % Suggest corrections
            suggest_corrections(Violations, Corrections),

            % Convert to JSON format
            findall(
                _{
                    entity: EntityStr,
                    predicate: PredStr,
                    suggestedObject: ObjStr,
                    confidence: Confidence
                },
                (
                    member(correction(Entity, Pred, Obj, Confidence), Corrections),
                    atom_string(Entity, EntityStr),
                    atom_string(Pred, PredStr),
                    atom_string(Obj, ObjStr)
                ),
                CorrectionDicts
            ),

            json_response(CorrectionDicts, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

%% =============================================
%% Helper Functions for WASM Interface
%% =============================================

% Convert a hierarchical tree to a JSON-friendly dictionary
hierarchy_tree_to_dict(tree(Root, Children), Dict) :-
    atom_string(Root, RootStr),
    maplist(hierarchy_tree_to_dict, Children, ChildrenDicts),
    Dict = _{
        type: RootStr,
        children: ChildrenDicts
    }.

% Convert a path to a string representation
path_to_string(Path, String) :-
    maplist(atom_string, Path, PathStrings),
    atomic_list_concat(PathStrings, '.', String).

% Convert a pattern to a string representation
pattern_to_string(Pattern, String) :-
    maplist(pattern_element_to_string, Pattern, ElementStrings),
    atomic_list_concat(ElementStrings, '.', String).

% Convert a pattern element to a string
pattern_element_to_string(pattern(Pred, Domain, Range), String) :-
    atom_string(Pred, PredStr),
    atom_string(Domain, DomainStr),
    atom_string(Range, RangeStr),
    format(string(String), '~w[~w->~w]', [PredStr, DomainStr, RangeStr]).

% Generate JSON response
json_response(Data, JsonString) :-
    atom_json_dict(JsonString, _{success: true, data: Data}, []).

% Generate JSON error
json_error(Code, Message, JsonString) :-
    atom_json_dict(JsonString, _{success: false, code: Code, message: Message}, []).
