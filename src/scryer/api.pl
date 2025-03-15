%% Predicate Discovery API for Category Theory-Based RDF Knowledge Engine
%% This module provides advanced predicate discovery capabilities
%% Date: March 14, 2025

:- module(cat_rdf_discovery, [
    % Path-based discovery predicates
    discover_potential_predicates/3,
    discover_path_patterns/3,
    analyze_path_frequency/2,
    find_missing_predicates/3,
    suggest_constraint_candidates/1,

    % Knowledge completion predicates
    complete_missing_paths/4,
    predict_path_validity/4,
    rank_paths_by_confidence/3,

    % WASM-friendly interfaces
    wasm_discover_potential_predicates/3,
    wasm_discover_path_patterns/3,
    wasm_analyze_path_frequency/2,
    wasm_find_missing_predicates/3,
    wasm_suggest_constraint_candidates/1,
    wasm_complete_missing_paths/4,
    wasm_predict_path_validity/4,
    wasm_rank_paths_by_confidence/3
]).

:- use_module(library(http/json)).
:- use_module(cat_rdf).  % Import the core module

%% =============================================
%% Path-based Predicate Discovery
%% =============================================

% Discover potential predicates between two entity types
discover_potential_predicates(SourceType, TargetType, Predicates) :-
    % Find all direct RDF predicates between instances of the given types
    findall(Predicate, (
        rdf(Source, rdf:type, SourceType, _),
        rdf(Target, rdf:type, TargetType, _),
        rdf(Source, Predicate, Target, _),
        % Exclude rdf:type and other system predicates
        \+ (Predicate = rdf:type)
    ), AllPredicates),

    % Remove duplicates and sort
    sort(AllPredicates, Predicates).

% Discover common path patterns in the knowledge base
discover_path_patterns(MaxLength, MinFrequency, PathPatterns) :-
    % Find all valid paths up to MaxLength
    findall(Path, (
        % Get all entity types
        rdf(_, rdf:type, SourceType, _),
        rdf(_, rdf:type, TargetType, _),
        % Only consider distinct type pairs
        SourceType @< TargetType,
        % Generate paths between these types
        generate_type_paths(SourceType, TargetType, MaxLength, _, ValidPaths),
        % Extract each path
        member(Path, ValidPaths)
    ), AllPaths),

    % Count frequency of each path pattern
    count_paths(AllPaths, PathCounts),

    % Filter by minimum frequency
    findall(Path-Count, (
        member(Path-Count, PathCounts),
        Count >= MinFrequency
    ), PathPatterns).

% Count occurrences of each path
count_paths(Paths, PathCounts) :-
    count_paths(Paths, [], PathCounts).

count_paths([], Acc, Sorted) :-
    sort(2, @>=, Acc, Sorted).  % Sort by count descending
count_paths([Path|Rest], Acc, Result) :-
    (selectchk(Path-Count, Acc, AccWithout) ->
        NewCount is Count + 1,
        NewAcc = [Path-NewCount|AccWithout]
    ;
        NewAcc = [Path-1|Acc]
    ),
    count_paths(Rest, NewAcc, Result).

% Analyze frequency of paths in the knowledge base
analyze_path_frequency(MaxLength, FrequencyStats) :-
    % Find all valid paths up to MaxLength
    findall(Path, (
        % Get all unique entity pairs
        rdf(Source, _, Target, _),
        Source \= Target,
        % Find valid paths between these entities
        find_valid_rdf_paths(Source, Target, MaxLength, _, ValidPaths),
        % Extract each path
        member(Path, ValidPaths)
    ), AllPaths),

    % Count paths by length
    findall(Length-Count, (
        between(1, MaxLength, Length),
        findall(Path, (
            member(Path, AllPaths),
            length(Path, Length)
        ), PathsOfLength),
        length(PathsOfLength, Count)
    ), FrequencyStats).

% Find potential missing predicates based on gold standard constraints
find_missing_predicates(SourceType, TargetType, MissingPredicates) :-
    % Find all entities of source type
    findall(Source, rdf(Source, rdf:type, SourceType, _), Sources),

    % Find all entities of target type
    findall(Target, rdf(Target, rdf:type, TargetType, _), Targets),

    % Find all gold constraints that might apply
    findall(SourcePred-TargetPred, gold_constraint(SourcePred, TargetPred, implication), Constraints),

    % For each source-target pair and constraint, check if source predicate exists but target doesn't
    findall(missing(Source, Target, TargetPred), (
        member(Source, Sources),
        member(Target, Targets),
        member(SourcePred-TargetPred, Constraints),
        rdf(Source, SourcePred, Target, _),
        \+ rdf(Source, TargetPred, Target, _)
    ), MissingPredicates).

% Suggest candidate constraints based on observed patterns
suggest_constraint_candidates(Suggestions) :-
    % Find all predicate pairs where one often implies the other
    findall(SourcePred-TargetPred-Confidence, (
        % Find all distinct predicates
        rdf(_, SourcePred, _, _),
        rdf(_, TargetPred, _, _),
        SourcePred \= TargetPred,

        % Count how often they co-occur
        findall(1, (
            rdf(S, SourcePred, O, _),
            rdf(S, TargetPred, O, _)
        ), CoOccurrences),
        length(CoOccurrences, CoOccurrenceCount),

        % Count total occurrences of source predicate
        findall(1, rdf(_, SourcePred, _, _), SourceOccurrences),
        length(SourceOccurrences, SourceCount),

        % Calculate confidence (how often target appears when source does)
        SourceCount > 0,
        Confidence is CoOccurrenceCount / SourceCount,
        Confidence > 0.7  % Only suggest high-confidence implications
    ), UnsortedSuggestions),

    % Sort by confidence descending
    sort(3, @>=, UnsortedSuggestions, Suggestions).

%% =============================================
%% Knowledge Completion
%% =============================================

% Complete missing paths based on gold standard constraints
complete_missing_paths(SourceType, TargetType, ConfidenceThreshold, CompletedPaths) :-
    % Find entities of given types
    findall(Source-Target, (
        rdf(Source, rdf:type, SourceType, _),
        rdf(Target, rdf:type, TargetType, _)
    ), EntityPairs),

    % Get all gold constraints
    findall(constraint(S, T, Type), gold_constraint(S, T, Type), GoldConstraints),

    % For each entity pair, find potential missing paths
    findall(Source-Target-Path-Confidence, (
        member(Source-Target, EntityPairs),

        % Generate all possible paths
        generate_type_paths(SourceType, TargetType, 3, _, PossiblePaths),
        member(Path, PossiblePaths),

        % Check if this path doesn't currently exist
        \+ check_path_validity(Source, Target, Path, _),

        % Predict confidence based on constraints
        predict_path_validity(Source, Target, Path, GoldConstraints, Confidence),
        Confidence >= ConfidenceThreshold
    ), CompletedPaths).

% Predict confidence score for a path based on constraints
predict_path_validity(Source, Target, Path, GoldConstraints, Confidence) :-
    % Count constraint violations for this path
    findall(1, (
        member(constraint(SourcePred, TargetPred, implication), GoldConstraints),
        path_instance_violates_constraint(Source, Target, Path, SourcePred, TargetPred)
    ), Violations),
    length(Violations, ViolationCount),

    % Count total applicable constraints
    findall(1, (
        member(constraint(SourcePred, _, _), GoldConstraints),
        (member(Morphism, Path),
         d_morphism(_, Morphism, _, _, _, Term),
         term_contains_predicate(Term, SourcePred))
    ), ApplicableConstraints),
    length(ApplicableConstraints, TotalApplicable),

    % Calculate confidence
    (TotalApplicable > 0 ->
        Confidence is 1 - (ViolationCount / TotalApplicable)
    ;
        % If no constraints apply, use medium confidence
        Confidence = 0.5
    ).

% Rank paths by confidence score
rank_paths_by_confidence(Paths, GoldConstraints, RankedPaths) :-
    % Score each path
    findall(Path-Confidence, (
        member(Path, Paths),
        validate_against_constraints(_, _, Path, GoldConstraints, Confidence)
    ), ScoredPaths),

    % Sort by confidence
    sort(2, @>=, ScoredPaths, RankedPaths).

%% =============================================
%% WASM-friendly Interfaces
%% =============================================

% WASM interface for discover_potential_predicates
wasm_discover_potential_predicates(SourceType, TargetType, Response) :-
    catch(
        (
            atom_string(SourceTypeAtom, SourceType),
            atom_string(TargetTypeAtom, TargetType),
            discover_potential_predicates(SourceTypeAtom, TargetTypeAtom, Predicates),

            % Convert predicate URIs to strings
            maplist(term_string, Predicates, PredicateStrings),
            json_response(PredicateStrings, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for discover_path_patterns
wasm_discover_path_patterns(MaxLength, MinFrequency, Response) :-
    catch(
        (
            discover_path_patterns(MaxLength, MinFrequency, PathPatterns),

            % Convert to JSON-friendly format
            findall(
                _{path: PathStr, frequency: Count},
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

% WASM interface for analyze_path_frequency
wasm_analyze_path_frequency(MaxLength, Response) :-
    catch(
        (
            analyze_path_frequency(MaxLength, FrequencyStats),

            % Convert to JSON-friendly format
            findall(
                _{length: Length, count: Count},
                member(Length-Count, FrequencyStats),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for find_missing_predicates
wasm_find_missing_predicates(SourceType, TargetType, Response) :-
    catch(
        (
            atom_string(SourceTypeAtom, SourceType),
            atom_string(TargetTypeAtom, TargetType),
            find_missing_predicates(SourceTypeAtom, TargetTypeAtom, MissingPredicates),

            % Convert to JSON-friendly format
            findall(
                _{
                    source: SourceStr,
                    target: TargetStr,
                    missingPredicate: PredStr
                },
                (
                    member(missing(Source, Target, Pred), MissingPredicates),
                    term_string(Source, SourceStr),
                    term_string(Target, TargetStr),
                    term_string(Pred, PredStr)
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for suggest_constraint_candidates
wasm_suggest_constraint_candidates(Response) :-
    catch(
        (
            suggest_constraint_candidates(Suggestions),

            % Convert to JSON-friendly format
            findall(
                _{
                    sourcePredicate: SourcePredStr,
                    targetPredicate: TargetPredStr,
                    confidence: Confidence
                },
                (
                    member(SourcePred-TargetPred-Confidence, Suggestions),
                    term_string(SourcePred, SourcePredStr),
                    term_string(TargetPred, TargetPredStr)
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for complete_missing_paths
wasm_complete_missing_paths(SourceType, TargetType, ConfidenceThreshold, Response) :-
    catch(
        (
            atom_string(SourceTypeAtom, SourceType),
            atom_string(TargetTypeAtom, TargetType),
            complete_missing_paths(SourceTypeAtom, TargetTypeAtom, ConfidenceThreshold, CompletedPaths),

            % Convert to JSON-friendly format
            findall(
                _{
                    source: SourceStr,
                    target: TargetStr,
                    path: PathStr,
                    confidence: Confidence
                },
                (
                    member(Source-Target-Path-Confidence, CompletedPaths),
                    term_string(Source, SourceStr),
                    term_string(Target, TargetStr),
                    path_to_string(Path, PathStr)
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for predict_path_validity
wasm_predict_path_validity(Source, Target, PathJson, Response) :-
    catch(
        (
            atom_string(SourceAtom, Source),
            atom_string(TargetAtom, Target),

            % Parse path from JSON
            atom_json_dict(PathJson, PathDict, []),
            get_dict(morphisms, PathDict, PathMorphismStrings),
            maplist(atom_string, PathMorphisms, PathMorphismStrings),

            % Get all gold constraints
            findall(constraint(S, T, Type), gold_constraint(S, T, Type), GoldConstraints),

            predict_path_validity(SourceAtom, TargetAtom, PathMorphisms, GoldConstraints, Confidence),

            json_response(Confidence, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% WASM interface for rank_paths_by_confidence
wasm_rank_paths_by_confidence(PathsJson, Response) :-
    catch(
        (
            % Parse paths from JSON
            atom_json_dict(PathsJson, PathDicts, []),
            maplist(dict_to_path, PathDicts, Paths),

            % Get all gold constraints
            findall(constraint(S, T, Type), gold_constraint(S, T, Type), GoldConstraints),

            rank_paths_by_confidence(Paths, GoldConstraints, RankedPaths),

            % Convert to JSON-friendly format
            findall(
                _{
                    path: PathStr,
                    confidence: Confidence
                },
                (
                    member(Path-Confidence, RankedPaths),
                    path_to_string(Path, PathStr)
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

%% =============================================
%% Helper Functions
%% =============================================

% Convert a path to a string representation
path_to_string(Path, String) :-
    atomic_list_concat(Path, '.', String).

% Generate a success JSON response
json_response(Data, JsonString) :-
    atom_json_dict(JsonString, _{success: true, data: Data}, []).

% Generate an error JSON response
json_error(Code, Message, JsonString) :-
    atom_json_dict(JsonString, _{success: false, code: Code, message: Message}, []).

% Convert a path dictionary to a path
dict_to_path(Dict, Path) :-
    get_dict(morphisms, Dict, PathMorphismStrings),
    maplist(atom_string, Path, PathMorphismStrings).



%% Prolog Predicate Handler for WASM Interface
%% Implementation for the refactored predicate-focused TypeScript API
%% Date: March 14, 2025

:- module(cat_rdf_predicate_handler, [
    % Unary predicates
    wasm_create_unary_predicate/2,
    wasm_get_predicate/2,
    wasm_update_predicate/2,
    wasm_delete_predicate/2,

    % Binary predicates
    wasm_create_binary_predicate/2,

    % Path predicates
    wasm_create_path_predicate/2,

    % Predicate sets
    wasm_create_predicate_set/2,
    wasm_get_predicate_set/2,
    wasm_update_predicate_set/2,
    wasm_delete_predicate_set/2,
    wasm_add_predicate_to_set/3,
    wasm_remove_predicate_from_set/3,

    % Assertions
    wasm_assert_predicate/2,
    wasm_retract_predicate/2,
    wasm_query_assertions/2,

    % Constraints
    wasm_define_constraint/2,
    wasm_get_constraint/2,
    wasm_update_constraint/2,
    wasm_delete_constraint/2,

    % Discovery
    wasm_discover_path_predicates/4,
    wasm_find_missing_predicates/2,
    wasm_suggest_constraints/2,

    % Evaluation
    wasm_evaluate_assertions/2,
    wasm_validate_constraints/2,
    wasm_complete_predicates/3
]).

:- use_module(library(http/json)).
:- use_module(cat_rdf).  % Core RDF module
:- use_module(cat_rdf_discovery).  % Discovery module

% Dynamic predicates for storing predicate information
:- dynamic unary_predicate/2.      % id, label
:- dynamic binary_predicate/5.     % id, label, domain, range, inverse
:- dynamic path_predicate/4.       % id, label, path_list, composition_term
:- dynamic predicate_set/3.        % id, name, description
:- dynamic predicate_set_member/2. % set_id, predicate_id
:- dynamic predicate_assertion/4.  % predicate_id, entity_ids_list, truth_value, confidence
:- dynamic logical_constraint/5.   % id, type, sources_list, targets_list, description

%% =============================================
%% Helper Functions
%% =============================================

% Generate a UUID
generate_uuid(UUID) :-
    % Simple implementation - in production use a proper UUID generator
    random_between(100000, 999999, Rand),
    get_time(Time),
    format(atom(UUID), 'pred-~w-~w', [Rand, Time]).

% Success response
json_response(Data, JsonString) :-
    atom_json_dict(JsonString, _{success: true, data: Data}, []).

% Error response
json_error(Code, Message, JsonString) :-
    atom_json_dict(JsonString, _{success: false, error: _{code: Code, message: Message}}, []).

% Parse JSON to dict
parse_json(JsonString, Dict) :-
    atom_json_dict(JsonString, Dict, []).

%% =============================================
%% Unary Predicates
%% =============================================

% Create a unary predicate
wasm_create_unary_predicate(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            generate_uuid(Id),
            get_dict(label, Data, Label),
            assertz(unary_predicate(Id, Label)),

            % Prepare response
            json_response(_{
                id: Id,
                type: 'unary',
                label: Label
            }, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Get any type of predicate
wasm_get_predicate(Id, Response) :-
    catch(
        (
            (
                % Try unary predicate
                unary_predicate(Id, Label) ->
                PredicateData = _{
                    id: Id,
                    type: 'unary',
                    label: Label
                }
            ;
                % Try binary predicate
                binary_predicate(Id, Label, Domain, Range, Inverse) ->
                PredicateData = _{
                    id: Id,
                    type: 'binary',
                    label: Label,
                    domain: Domain,
                    range: Range,
                    inverse: Inverse
                }
            ;
                % Try path predicate
                path_predicate(Id, Label, PathList, _) ->
                PredicateData = _{
                    id: Id,
                    type: 'path',
                    label: Label,
                    path: PathList
                }
            ;
                % Predicate not found
                throw('Predicate not found')
            ),

            json_response(PredicateData, Response)
        ),
        Error,
        json_error(404, Error, Response)
    ).

% Update any type of predicate
wasm_update_predicate(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            get_dict(id, Data, Id),
            get_dict(type, Data, Type),

            % Handle different predicate types
            (Type = 'unary' ->
                retractall(unary_predicate(Id, _)),
                get_dict(label, Data, Label),
                assertz(unary_predicate(Id, Label)),
                PredicateData = _{
                    id: Id,
                    type: Type,
                    label: Label
                }
            ; Type = 'binary' ->
                retractall(binary_predicate(Id, _, _, _, _)),
                get_dict(label, Data, Label),
                get_dict(domain, Data, Domain),
                get_dict(range, Data, Range),
                get_dict(inverse, Data, Inverse),
                assertz(binary_predicate(Id, Label, Domain, Range, Inverse)),
                PredicateData = _{
                    id: Id,
                    type: Type,
                    label: Label,
                    domain: Domain,
                    range: Range,
                    inverse: Inverse
                }
            ; Type = 'path' ->
                retractall(path_predicate(Id, _, _, _)),
                get_dict(label, Data, Label),
                get_dict(path, Data, Path),

                % Reconstruct the composition term
                path_to_composition_term(Path, CompTerm),

                assertz(path_predicate(Id, Label, Path, CompTerm)),
                PredicateData = _{
                    id: Id,
                    type: Type,
                    label: Label,
                    path: Path
                }
            ;
                throw('Unknown predicate type')
            ),

            json_response(PredicateData, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Delete any type of predicate
wasm_delete_predicate(Id, Response) :-
    catch(
        (
            % Try to retract from all predicate types
            (retractall(unary_predicate(Id, _)) ; true),
            (retractall(binary_predicate(Id, _, _, _, _)) ; true),
            (retractall(path_predicate(Id, _, _, _)) ; true),

            % Also remove from any predicate sets
            retractall(predicate_set_member(_, Id)),

            % And remove any assertions
            retractall(predicate_assertion(Id, _, _, _)),

            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

%% =============================================
%% Binary Predicates
%% =============================================

% Create a binary predicate
wasm_create_binary_predicate(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            generate_uuid(Id),
            get_dict(label, Data, Label),
            get_dict(domain, Data, Domain),
            get_dict(range, Data, Range),
            (get_dict(inverse, Data, Inverse) -> true ; Inverse = ''),

            assertz(binary_predicate(Id, Label, Domain, Range, Inverse)),

            % Also create a corresponding morphism in the category model
            prolog_term_for_binary_predicate(Id, Label, PrologTerm),
            define_d_morphism('kb', Id, Domain, Range, Label, PrologTerm),

            % Prepare response
            json_response(_{
                id: Id,
                type: 'binary',
                label: Label,
                domain: Domain,
                range: Range,
                inverse: Inverse
            }, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Generate a Prolog term for a binary predicate
prolog_term_for_binary_predicate(Id, _, PrologTerm) :-
    PrologTerm = rdf_direct_relation(S, O, Id, G).

%% =============================================
%% Path Predicates
%% =============================================

% Create a path predicate
wasm_create_path_predicate(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            generate_uuid(Id),
            get_dict(label, Data, Label),
            get_dict(path, Data, Path),
            get_dict(domain, Data, Domain),
            get_dict(range, Data, Range),

            % Convert path to a composition term
            path_to_composition_term(Path, CompTerm),

            assertz(path_predicate(Id, Label, Path, CompTerm)),

            % Also create a corresponding path in the category model
            define_d_morphism('kb', Id, Domain, Range, Label, CompTerm),

            % Prepare response
            json_response(_{
                id: Id,
                type: 'path',
                label: Label,
                path: Path,
                domain: Domain,
                range: Range
            }, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Convert a path to a composition term
path_to_composition_term([SinglePred], CompTerm) :-
    % Single predicate - use its term directly
    binary_predicate(SinglePred, _, _, _, _),
    prolog_term_for_binary_predicate(SinglePred, _, CompTerm).

path_to_composition_term([First|Rest], CompTerm) :-
    % Multiple predicates - build a composition
    path_to_composition_term(Rest, RestTerm),
    CompTerm = compose_predicates(First, RestTerm).

%% =============================================
%% Predicate Sets
%% =============================================

% Create a predicate set
wasm_create_predicate_set(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            generate_uuid(Id),
            get_dict(name, Data, Name),
            (get_dict(description, Data, Description) -> true ; Description = ''),
            (get_dict(predicates, Data, Predicates) -> true ; Predicates = []),

            assertz(predicate_set(Id, Name, Description)),

            % Add predicates to the set
            maplist(add_to_set(Id), Predicates),

            % Prepare response
            json_response(_{
                id: Id,
                name: Name,
                description: Description,
                predicates: Predicates
            }, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Helper to add a predicate to a set
add_to_set(SetId, PredId) :-
    assertz(predicate_set_member(SetId, PredId)).

% Get a predicate set
wasm_get_predicate_set(Id, Response) :-
    catch(
        (
            predicate_set(Id, Name, Description),

            % Get all predicates in this set
            findall(PredId, predicate_set_member(Id, PredId), Predicates),

            json_response(_{
                id: Id,
                name: Name,
                description: Description,
                predicates: Predicates
            }, Response)
        ),
        Error,
        json_error(404, 'Predicate set not found', Response)
    ).

% Update a predicate set
wasm_update_predicate_set(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            get_dict(id, Data, Id),

            % Check if set exists
            (predicate_set(Id, _, _) ->
                retractall(predicate_set(Id, _, _)),
                get_dict(name, Data, Name),
                (get_dict(description, Data, Description) -> true ; Description = ''),
                assertz(predicate_set(Id, Name, Description)),

                % Update predicates if provided
                (get_dict(predicates, Data, Predicates) ->
                    % Clear existing members and add new ones
                    retractall(predicate_set_member(Id, _)),
                    maplist(add_to_set(Id), Predicates)
                ; true),

                % Get updated predicates list
                findall(PredId, predicate_set_member(Id, PredId), UpdatedPredicates),

                json_response(_{
                    id: Id,
                    name: Name,
                    description: Description,
                    predicates: UpdatedPredicates
                }, Response)
            ;
                throw('Predicate set not found')
            )
        ),
        Error,
        json_error(404, Error, Response)
    ).

% Delete a predicate set
wasm_delete_predicate_set(Id, Response) :-
    catch(
        (
            retractall(predicate_set(Id, _, _)),
            retractall(predicate_set_member(Id, _)),

            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Add a predicate to a set
wasm_add_predicate_to_set(SetId, PredId, Response) :-
    catch(
        (
            % Check if set exists
            (predicate_set(SetId, _, _) ->
                % Check if predicate exists in any form
                ((unary_predicate(PredId, _) ; binary_predicate(PredId, _, _, _, _) ; path_predicate(PredId, _, _, _)) ->
                    % Check if already in set
                    (predicate_set_member(SetId, PredId) ->
                        % Already a member
                        true
                    ;
                        % Add to set
                        assertz(predicate_set_member(SetId, PredId))
                    ),

                    % Get the updated set
                    predicate_set(SetId, Name, Description),
                    findall(P, predicate_set_member(SetId, P), Predicates),

                    json_response(_{
                        id: SetId,
                        name: Name,
                        description: Description,
                        predicates: Predicates
                    }, Response)
                ;
                    throw('Predicate not found')
                )
            ;
                throw('Predicate set not found')
            )
        ),
        Error,
        json_error(404, Error, Response)
    ).

% Remove a predicate from a set
wasm_remove_predicate_from_set(SetId, PredId, Response) :-
    catch(
        (
            % Check if set exists
            (predicate_set(SetId, _, _) ->
                % Remove predicate from set
                retractall(predicate_set_member(SetId, PredId)),

                % Get the updated set
                predicate_set(SetId, Name, Description),
                findall(P, predicate_set_member(SetId, P), Predicates),

                json_response(_{
                    id: SetId,
                    name: Name,
                    description: Description,
                    predicates: Predicates
                }, Response)
            ;
                throw('Predicate set not found')
            )
        ),
        Error,
        json_error(404, Error, Response)
    ).

%% =============================================
%% Predicate Assertions
%% =============================================

% Assert a predicate
wasm_assert_predicate(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            get_dict(predicateId, Data, PredId),
            get_dict(entityIds, Data, EntityIds),
            get_dict(truthValue, Data, TruthValue),
            (get_dict(confidence, Data, Confidence) -> true ; Confidence = 1.0),

            % Check predicate type and validate entity count
            validate_predicate_assertion(PredId, EntityIds),

            % Remove any existing assertions for this predicate and entities
            retractall(predicate_assertion(PredId, EntityIds, _, _)),

            % Add the new assertion
            assertz(predicate_assertion(PredId, EntityIds, TruthValue, Confidence)),

            % Also assert in the RDF store if it's a binary or path predicate
            (
                (binary_predicate(PredId, _, _, _, _) ; path_predicate(PredId, _, _, _)),
                EntityIds = [Subject, Object] ->

                % Convert to RDF assertion
                (TruthValue = true ->
                    % Add RDF triple
                    rdf_assert(Subject, PredId, Object)
                ;
                    % Remove RDF triple for false assertions
                    rdf_retractall(Subject, PredId, Object)
                )
            ; true),

            % Return the assertion
            json_response(_{
                predicateId: PredId,
                entityIds: EntityIds,
                truthValue: TruthValue,
                confidence: Confidence
            }, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Validate that entity count matches predicate type
validate_predicate_assertion(PredId, EntityIds) :-
    % Check predicate type
    (
        unary_predicate(PredId, _) ->
        % Unary predicates should have exactly one entity
        length(EntityIds, 1)
    ;
        (binary_predicate(PredId, _, _, _, _) ; path_predicate(PredId, _, _, _)) ->
        % Binary and path predicates should have exactly two entities
        length(EntityIds, 2)
    ;
        % Unknown predicate type
        throw('Invalid predicate type')
    ).

% Retract a predicate assertion
wasm_retract_predicate(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            get_dict(predicateId, Data, PredId),
            get_dict(entityIds, Data, EntityIds),

            % Remove the assertion
            retractall(predicate_assertion(PredId, EntityIds, _, _)),

            % Also retract from RDF store if binary or path
            (
                (binary_predicate(PredId, _, _, _, _) ; path_predicate(PredId, _, _, _)),
                EntityIds = [Subject, Object] ->
                rdf_retractall(Subject, PredId, Object)
            ; true),

            json_response(true, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Query predicate assertions
wasm_query_assertions(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Filters),

            % Extract filters if present
            (get_dict(predicateId, Filters, FilterPredId) -> true ; FilterPredId = _),
            (get_dict(entityIds, Filters, FilterEntityIds) -> true ; FilterEntityIds = _),
            (get_dict(truthValue, Filters, FilterTruthValue) -> true ; FilterTruthValue = _),

            % Query assertions based on filters
            findall(
                _{
                    predicateId: PredId,
                    entityIds: EntityIds,
                    truthValue: TruthValue,
                    confidence: Confidence
                },
                (
                    predicate_assertion(PredId, EntityIds, TruthValue, Confidence),
                    % Apply filters
                    (FilterPredId = _ ; FilterPredId = PredId),
                    (FilterEntityIds = _ ; FilterEntityIds = EntityIds),
                    (FilterTruthValue = _ ; FilterTruthValue = TruthValue)
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

%% =============================================
%% Logical Constraints
%% =============================================

% Define a logical constraint
wasm_define_constraint(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            generate_uuid(Id),
            get_dict(type, Data, Type),
            get_dict(sourcePredicateIds, Data, SourcePredicates),
            get_dict(targetPredicateIds, Data, TargetPredicates),
            (get_dict(description, Data, Description) -> true ; Description = ''),

            % Add the constraint
            assertz(logical_constraint(Id, Type, SourcePredicates, TargetPredicates, Description)),

            % Add to the gold standard constraints for evaluation
            maplist(add_gold_constraint(Type), SourcePredicates, TargetPredicates),

            json_response(_{
                id: Id,
                type: Type,
                sourcePredicateIds: SourcePredicates,
                targetPredicateIds: TargetPredicates,
                description: Description
            }, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Add a gold constraint for each source-target pair
add_gold_constraint(Type, SourcePred, TargetPred) :-
    % Map constraint type to Prolog constraint type
    (Type = 'implication' ->
        ConstraintType = implication
    ; Type = 'equivalence' ->
        ConstraintType = equivalence
    ; Type = 'mutual_exclusion' ->
        ConstraintType = mutual_exclusion
    ; Type = 'conjunction' ->
        ConstraintType = conjunction
    ;
        ConstraintType = implication  % Default
    ),

    % Define the constraint in the gold standard model
    define_gold_constraint(SourcePred, TargetPred, ConstraintType).

% Get a logical constraint
wasm_get_constraint(Id, Response) :-
    catch(
        (
            logical_constraint(Id, Type, SourcePredicates, TargetPredicates, Description),

            json_response(_{
                id: Id,
                type: Type,
                sourcePredicateIds: SourcePredicates,
                targetPredicateIds: TargetPredicates,
                description: Description
            }, Response)
        ),
        Error,
        json_error(404, 'Constraint not found', Response)
    ).

% Update a logical constraint
wasm_update_constraint(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Data),
            get_dict(id, Data, Id),

            % Check if constraint exists
            (logical_constraint(Id, _, _, _, _) ->
                % Remove existing constraint
                retractall(logical_constraint(Id, _, _, _, _)),

                % Add updated constraint
                get_dict(type, Data, Type),
                get_dict(sourcePredicateIds, Data, SourcePredicates),
                get_dict(targetPredicateIds, Data, TargetPredicates),
                (get_dict(description, Data, Description) -> true ; Description = ''),

                assertz(logical_constraint(Id, Type, SourcePredicates, TargetPredicates, Description)),

                % Update gold constraints (first remove old ones then add new ones)
                % Note: This is simplified; in practice, you'd need to track the old constraint pairs
                % and only update what's changed to avoid duplicates
                retractall(gold_constraint(_, _, _)),
                maplist(add_gold_constraint(Type), SourcePredicates, TargetPredicates),

                json_response(_{
                    id: Id,
                    type: Type,
                    sourcePredicateIds: SourcePredicates,
                    targetPredicateIds: TargetPredicates,
                    description: Description
                }, Response)
            ;
                throw('Constraint not found')
            )
        ),
        Error,
        json_error(404, Error, Response)
    ).

% Delete a logical constraint
wasm_delete_constraint(Id, Response) :-
    catch(
        (
            % Get the constraint details first (to remove gold constraints)
            (logical_constraint(Id, Type, SourcePredicates, TargetPredicates, _) ->
                % Remove the constraint
                retractall(logical_constraint(Id, _, _, _, _)),

                % Remove corresponding gold constraints
                % Note: This is simplified; in practice you might need to check if other
                % logical constraints also use these pairs before removing them
                maplist(remove_gold_constraint, SourcePredicates, TargetPredicates),

                json_response(true, Response)
            ;
                throw('Constraint not found')
            )
        ),
        Error,
        json_error(404, Error, Response)
    ).

% Remove a gold constraint
remove_gold_constraint(SourcePred, TargetPred) :-
    retractall(gold_constraint(SourcePred, TargetPred, _)).

%% =============================================
%% Predicate Discovery
%% =============================================

% Discover path predicates
wasm_discover_path_predicates(Domain, Range, MaxLength, Response) :-
    catch(
        (
            atom_string(DomainAtom, Domain),
            atom_string(RangeAtom, Range),

            % Use the path generator from the core module
            generate_paths(DomainAtom, RangeAtom, MaxLength, Paths),

            % Convert to path predicates
            findall(
                _{
                    id: Id,
                    type: 'path',
                    label: Label,
                    path: Path,
                    domain: Domain,
                    range: Range
                },
                (
                    member(Path, Paths),
                    % Generate a label for the path
                    atomic_list_concat(Path, '.', PathStr),
                    atom_concat('path_', PathStr, Label),
                    % Generate an ID
                    generate_uuid(Id)
                    % Note: Not actually saving these as they're just suggestions
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Find missing predicates for an entity
wasm_find_missing_predicates(EntityId, Response) :-
    catch(
        (
            atom_string(EntityAtom, EntityId),

            % Get all predicates that the entity participates in
            findall(PredId, predicate_assertion(PredId, [EntityAtom|_], true, _), AssertedPreds),

            % Get all constraints where a source predicate is asserted
            findall(
                MissingPred,
                (
                    logical_constraint(_, 'implication', SourcePreds, TargetPreds, _),
                    member(SourcePred, SourcePreds),
                    member(SourcePred, AssertedPreds),
                    member(TargetPred, TargetPreds),
                    \+ member(TargetPred, AssertedPreds),
                    % This is a potentially missing predicate
                    MissingPred = TargetPred
                ),
                MissingPredIds
            ),

            % Get full predicate details for each missing predicate
            findall(
                PredData,
                (
                    member(PredId, MissingPredIds),
                    (
                        unary_predicate(PredId, Label) ->
                        PredData = _{
                            id: PredId,
                            type: 'unary',
                            label: Label
                        }
                    ;
                        binary_predicate(PredId, Label, Domain, Range, Inverse) ->
                        PredData = _{
                            id: PredId,
                            type: 'binary',
                            label: Label,
                            domain: Domain,
                            range: Range,
                            inverse: Inverse
                        }
                    ;
                        path_predicate(PredId, Label, Path, _) ->
                        PredData = _{
                            id: PredId,
                            type: 'path',
                            label: Label,
                            path: Path
                        }
                    )
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Suggest constraints based on data patterns
wasm_suggest_constraints(MinConfidence, Response) :-
    catch(
        (
            % Use the constraint suggestion predicate from discovery module
            suggest_constraint_candidates(Suggestions),

            % Filter by confidence
            findall(
                _{
                    type: 'implication',
                    sourcePredicateIds: [SourcePred],
                    targetPredicateIds: [TargetPred],
                    confidence: Confidence,
                    description: Description
                },
                (
                    member(SourcePred-TargetPred-Confidence, Suggestions),
                    Confidence >= MinConfidence,
                    % Create a description
                    format(atom(Description), 'Suggested implication from ~w to ~w with confidence ~2f',
                           [SourcePred, TargetPred, Confidence])
                ),
                Results
            ),

            json_response(Results, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

%% =============================================
%% Evaluation and Validation
%% =============================================

% Evaluate assertions using gold standard metrics
wasm_evaluate_assertions(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Assertions),

            % Convert assertions to Prolog format for evaluation
            assertions_to_predictions(Assertions, Predictions),

            % Get all gold facts
            findall(gold(P, E, T), gold_fact(P, E, T), GoldFacts),

            % Evaluate accuracy
            evaluate_accuracy(Predictions, GoldFacts, F1Score),

            % Prepare beliefs for consistency check
            assertions_to_beliefs(Assertions, Beliefs),

            % Get all constraints
            findall(constraint(S, T, Type), gold_constraint(S, T, Type), Constraints),

            % Measure consistency
            measure_consistency(Beliefs, Constraints, ConsistencyScore),

            % Get violated constraints
            findall(
                _{
                    type: Type,
                    sourcePredicateIds: [S],
                    targetPredicateIds: [T]
                },
                (
                    member(belief(E, S, true), Beliefs),
                    member(belief(E, T, false), Beliefs),
                    gold_constraint(S, T, Type)
                ),
                ViolatedConstraints
            ),

            json_response(_{
                accuracy: F1Score,
                consistency: ConsistencyScore,
                violatedConstraints: ViolatedConstraints
            }, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Convert assertion list to predictions
assertions_to_predictions(Assertions, Predictions) :-
    maplist(assertion_to_prediction, Assertions, Predictions).

assertion_to_prediction(Assertion, pred(PredId, EntityId, TruthValue)) :-
    get_dict(predicateId, Assertion, PredId),
    get_dict(entityIds, Assertion, EntityIds),
    get_dict(truthValue, Assertion, TruthValue),
    % For simplicity, use first entity for unary evaluation
    EntityIds = [EntityId|_].

% Convert assertion list to beliefs
assertions_to_beliefs(Assertions, Beliefs) :-
    maplist(assertion_to_belief, Assertions, Beliefs).

assertion_to_belief(Assertion, belief(EntityId, PredId, TruthValue)) :-
    get_dict(predicateId, Assertion, PredId),
    get_dict(entityIds, Assertion, EntityIds),
    get_dict(truthValue, Assertion, TruthValue),
    % For simplicity, use first entity
    EntityIds = [EntityId|_].

% Validate constraints for a set of assertions
wasm_validate_constraints(JsonData, Response) :-
    catch(
        (
            parse_json(JsonData, Assertions),

            % Convert assertions to beliefs
            assertions_to_beliefs(Assertions, Beliefs),

            % Get all logical constraints
            findall(
                constraint(Id, Type, SourcePreds, TargetPreds),
                logical_constraint(Id, Type, SourcePreds, TargetPreds, _),
                LogicalConstraints
            ),

            % Check each constraint
            findall(
                ValidConstraint,
                (
                    member(constraint(Id, Type, SourcePreds, TargetPreds), LogicalConstraints),
                    constraint_satisfied(Type, SourcePreds, TargetPreds, Beliefs),
                    logical_constraint(Id, Type, SourcePreds, TargetPreds, Description),
                    ValidConstraint = _{
                        id: Id,
                        type: Type,
                        sourcePredicateIds: SourcePreds,
                        targetPredicateIds: TargetPreds,
                        description: Description
                    }
                ),
                ValidConstraints
            ),

            % Find violated constraints
            findall(
                ViolatedConstraint,
                (
                    member(constraint(Id, Type, SourcePreds, TargetPreds), LogicalConstraints),
                    \+ constraint_satisfied(Type, SourcePreds, TargetPreds, Beliefs),
                    logical_constraint(Id, Type, SourcePreds, TargetPreds, Description),
                    ViolatedConstraint = _{
                        id: Id,
                        type: Type,
                        sourcePredicateIds: SourcePreds,
                        targetPredicateIds: TargetPreds,
                        description: Description
                    }
                ),
                ViolatedConstraints
            ),

            json_response(_{
                validConstraints: ValidConstraints,
                violatedConstraints: ViolatedConstraints
            }, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).

% Check if a constraint is satisfied
constraint_satisfied('implication', SourcePreds, TargetPreds, Beliefs) :-
    % For simplicity, we'll just handle the first predicate in each list
    SourcePreds = [SourcePred|_],
    TargetPreds = [TargetPred|_],

    % ∀x (P(x) → Q(x)) is satisfied if for all entities, if P(x) is true, then Q(x) is true
    \+ (
        member(belief(Entity, SourcePred, true), Beliefs),
        member(belief(Entity, TargetPred, false), Beliefs)
    ).

constraint_satisfied('equivalence', SourcePreds, TargetPreds, Beliefs) :-
    % For equivalence, both implications must hold
    constraint_satisfied('implication', SourcePreds, TargetPreds, Beliefs),
    constraint_satisfied('implication', TargetPreds, SourcePreds, Beliefs).

constraint_satisfied('mutual_exclusion', SourcePreds, TargetPreds, Beliefs) :-
    % For mutual exclusion, they can't both be true
    SourcePreds = [SourcePred|_],
    TargetPreds = [TargetPred|_],

    \+ (
        member(belief(Entity, SourcePred, true), Beliefs),
        member(belief(Entity, TargetPred, true), Beliefs)
    ).

constraint_satisfied('conjunction', SourcePreds, TargetPreds, Beliefs) :-
    % Conjunction is satisfied if all source predicates being true implies target is true
    TargetPreds = [TargetPred|_],

    \+ (
        % Check if all source predicates are true for an entity
        (
            member(belief(Entity, _, _), Beliefs),  % Get all entities with beliefs
            forall(
                member(SourcePred, SourcePreds),
                member(belief(Entity, SourcePred, true), Beliefs)
            )
        ),
        % But the target predicate is false
        member(belief(Entity, TargetPred, false), Beliefs)
    ).

% Complete predicates for an entity
wasm_complete_predicates(EntityId, ConfidenceThreshold, Response) :-
    catch(
        (
            atom_string(EntityAtom, EntityId),

            % Find missing predicates
            findall(
                _{
                    predicateId: MissingPredId,
                    entityIds: [EntityAtom],
                    truthValue: true,
                    confidence: Confidence
                },
                (
                    % Find all predicates that the entity has asserted
                    predicate_assertion(AssertedPredId, [EntityAtom|_], true, _),

                    % Find constraints where this is the source
                    logical_constraint(_, 'implication', [AssertedPredId], [MissingPredId], _),

                    % Check if the target predicate is not already asserted
                    \+ predicate_assertion(MissingPredId, [EntityAtom|_], _, _),

                    % Calculate confidence based on constraint evidence
                    % (simplified - in practice you'd use a more sophisticated approach)
                    Confidence = 0.9,  % Default high confidence for implication constraints

                    % Only include if above threshold
                    Confidence >= ConfidenceThreshold
                ),
                UnaryCompletions
            ),

            % For binary predicates, we need to find related entities
            findall(
                _{
                    predicateId: MissingPredId,
                    entityIds: [EntityAtom, RelatedEntity],
                    truthValue: true,
                    confidence: Confidence
                },
                (
                    % Find binary relations where this entity is the subject
                    predicate_assertion(AssertedPredId, [EntityAtom, RelatedEntity], true, _),
                    binary_predicate(AssertedPredId, _, _, _, _),

                    % Find constraints where this is the source
                    logical_constraint(_, 'implication', [AssertedPredId], [MissingPredId], _),
                    binary_predicate(MissingPredId, _, _, _, _),

                    % Check if the target relation is not already asserted
                    \+ predicate_assertion(MissingPredId, [EntityAtom, RelatedEntity], _, _),

                    % Calculate confidence
                    Confidence = 0.85,  % Slightly lower for binary relations

                    % Only include if above threshold
                    Confidence >= ConfidenceThreshold
                ),
                BinaryCompletions
            ),

            % Combine unary and binary completions
            append(UnaryCompletions, BinaryCompletions, Completions),

            json_response(Completions, Response)
        ),
        Error,
        json_error(500, Error, Response)
    ).
