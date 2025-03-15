// Example Application Using Predicate-Focused API
// Demonstrates how to use the TypeScript API for the Category Theory-Based RDF Knowledge Engine

import {
    PredicateKnowledgeEngine,
    Predicate,
    PredicateInstance,
    LogicalConstraint,
    ConstraintGraph,
    ConstraintType,
    GoldFact,
    PredicateQueryParams,
    PredicateDiscoveryParams,
    URI
} from './predicate-focused-api';
import { initScryerProlog } from './wasm-bridge';

/**
 * Main demo application
 */
async function main() {
    try {
        console.log("Initializing Category Theory-Based RDF Knowledge Engine...");

        // Initialize Scryer Prolog
        const prologInstance = await initScryerProlog();

        // Create engine instance
        const engine = new PredicateKnowledgeEngine(prologInstance);

        // Initialize with data directory
        const initResult = await engine.initialize('/data/rdf');
        if (!initResult.success) {
            throw new Error(`Initialization failed: ${initResult.message}`);
        }

        console.log("Engine initialized successfully");

        // Define a knowledge graph for an organization
        await defineOrganizationKnowledgeGraph(engine);

        // Define gold standard constraints
        await defineConstraints(engine);

        // Query the knowledge graph
        await performQueries(engine);

        // Demonstrate predicate discovery features
        await discoverPredicates(engine);

        // Evaluate consistency of the knowledge graph
        await evaluateKnowledgeGraph(engine);

        console.log("Demo completed successfully");
    } catch (error) {
        console.error("Demo failed:", error);
    }
}

/**
 * Define predicates and instances for an organization knowledge graph
 */
async function defineOrganizationKnowledgeGraph(engine: PredicateKnowledgeEngine) {
    console.log("\n--- Defining Organization Knowledge Graph ---");

    // Define predicates
    const predicates: Predicate[] = [
        {
            uri: 'ex:worksAt',
            label: 'works at',
            domain: 'ex:Person',
            range: 'ex:Organization',
            transitive: false
        },
        {
            uri: 'ex:locatedIn',
            label: 'located in',
            domain: 'ex:Organization',
            range: 'ex:Location',
            transitive: true
        },
        {
            uri: 'ex:belongsTo',
            label: 'belongs to',
            domain: 'ex:Location',
            range: 'ex:Country',
            transitive: false
        },
        {
            uri: 'ex:worksIn',
            label: 'works in',
            domain: 'ex:Person',
            range: 'ex:Location',
            transitive: false
        },
        {
            uri: 'ex:managerOf',
            label: 'manager of',
            domain: 'ex:Person',
            range: 'ex:Person',
            transitive: false,
            symmetric: false
        },
        {
            uri: 'ex:colleagueOf',
            label: 'colleague of',
            domain: 'ex:Person',
            range: 'ex:Person',
            transitive: false,
            symmetric: true
        }
    ];

    // Create predicates
    for (const predicate of predicates) {
        const result = await engine.createPredicate(predicate);
        if (result.success) {
            console.log(`Created predicate: ${predicate.label}`);
        } else {
            console.warn(`Failed to create predicate ${predicate.uri}: ${result.message}`);
        }
    }

    // Create a predicate set for organization relationships
    const organizationSet = {
        id: 'ex:organizationRelations',
        name: 'Organization Relations',
        description: 'Predicates related to organizational structure',
        predicates: [
            'ex:worksAt',
            'ex:locatedIn',
            'ex:managerOf',
            'ex:colleagueOf'
        ]
    };

    const setResult = await engine.createPredicateSet(organizationSet);
    if (setResult.success) {
        console.log(`Created predicate set: ${organizationSet.name}`);
    }

    // Define predicate instances
    const instances: PredicateInstance[] = [
        // People working at organizations
        { subject: 'ex:john', predicate: 'ex:worksAt', object: 'ex:acme' },
        { subject: 'ex:alice', predicate: 'ex:worksAt', object: 'ex:acme' },
        { subject: 'ex:bob', predicate: 'ex:worksAt', object: 'ex:globex' },
        { subject: 'ex:charlie', predicate: 'ex:worksAt', object: 'ex:acme' },

        // Organization locations
        { subject: 'ex:acme', predicate: 'ex:locatedIn', object: 'ex:newYork' },
        { subject: 'ex:globex', predicate: 'ex:locatedIn', object: 'ex:sanFrancisco' },

        // Locations in countries
        { subject: 'ex:newYork', predicate: 'ex:belongsTo', object: 'ex:usa' },
        { subject: 'ex:sanFrancisco', predicate: 'ex:belongsTo', object: 'ex:usa' },

        // Management relationships
        { subject: 'ex:alice', predicate: 'ex:managerOf', object: 'ex:john' },
        { subject: 'ex:alice', predicate: 'ex:managerOf', object: 'ex:charlie' },

        // Colleague relationships
        { subject: 'ex:john', predicate: 'ex:colleagueOf', object: 'ex:charlie' },

        // Explicitly defined inferred relationship (for testing)
        { subject: 'ex:alice', predicate: 'ex:worksIn', object: 'ex:newYork' }
    ];

    // Add instances
    for (const instance of instances) {
        const result = await engine.addPredicateInstance(instance);
        if (!result.success) {
            console.warn(`Failed to add instance ${instance.subject} ${instance.predicate} ${instance.object}: ${result.message}`);
        }
    }

    console.log(`Added ${instances.length} predicate instances`);
}

/**
 * Define gold standard constraints for evaluation
 */
async function defineConstraints(engine: PredicateKnowledgeEngine) {
    console.log("\n--- Defining Gold Standard Constraints ---");

    // Define logical constraints
    const constraints: LogicalConstraint[] = [
        {
            id: 'constraint1',
            sourcePredicateUri: 'ex:worksAt',
            targetPredicateUri: 'ex:worksIn',
            constraintType: ConstraintType.IMPLICATION,
            weight: 1.0
        },
        {
            id: 'constraint2',
            sourcePredicateUri: 'ex:managerOf',
            targetPredicateUri: 'ex:colleagueOf',
            constraintType: ConstraintType.IMPLICATION,
            weight: 0.8
        },
        {
            id: 'constraint3',
            sourcePredicateUri: 'ex:colleagueOf',
            targetPredicateUri: 'ex:worksAt',
            constraintType: ConstraintType.IMPLICATION,
            weight: 0.9
        }
    ];

    // Define constraints
    for (const constraint of constraints) {
        const result = await engine.defineLogicalConstraint(constraint);
        if (result.success) {
            console.log(`Defined constraint: ${constraint.sourcePredicateUri} -> ${constraint.targetPredicateUri}`);
        } else {
            console.warn(`Failed to define constraint: ${result.message}`);
        }
    }

    // Create constraint graph
    const graph: ConstraintGraph = {
        id: 'graph1',
        constraints: constraints,
        description: 'Organization domain constraints'
    };

    const graphResult = await engine.createConstraintGraph(graph);
    if (graphResult.success) {
        console.log(`Created constraint graph: ${graph.id}`);
    }

    // Define gold standard facts
    const facts: GoldFact[] = [
        { predicate: 'ex:worksAt', entity: 'ex:john', truthValue: true },
        { predicate: 'ex:worksIn', entity: 'ex:john', truthValue: true },
        { predicate: 'ex:worksAt', entity: 'ex:alice', truthValue: true },
        { predicate: 'ex:worksIn', entity: 'ex:alice', truthValue: true },
        { predicate: 'ex:colleagueOf', entity: 'ex:john', truthValue: true },
        { predicate: 'ex:colleagueOf', entity: 'ex:charlie', truthValue: true }
    ];

    // Define facts
    for (const fact of facts) {
        const result = await engine.defineGoldFact(fact);
        if (!result.success) {
            console.warn(`Failed to define gold fact: ${result.message}`);
        }
    }

    console.log(`Defined ${facts.length} gold standard facts`);
}

/**
 * Perform queries on the knowledge graph
 */
async function performQueries(engine: PredicateKnowledgeEngine) {
    console.log("\n--- Performing Queries ---");

    // Query all employees of Acme
    const acmeEmployeesParams: PredicateQueryParams = {
        predicate: 'ex:worksAt',
        object: 'ex:acme'
    };

    const acmeResult = await engine.queryPredicateInstances(acmeEmployeesParams);
    if (acmeResult.success) {
        console.log(`Acme employees: ${acmeResult.data.items.map(i => i.subject).join(', ')}`);
    }

    // Query all management relationships
    const managerParams: PredicateQueryParams = {
        predicate: 'ex:managerOf'
    };

    const managerResult = await engine.queryPredicateInstances(managerParams);
    if (managerResult.success) {
        console.log("Management relationships:");
        managerResult.data.items.forEach(i => {
            console.log(`  ${i.subject} manages ${i.object}`);
        });
    }

    // Parse and execute a path query: works_at.located_in
    const pathExpression = "works_at.located_in";
    const parsedPath = await engine.parsePathExpression(pathExpression);

    if (parsedPath.success) {
        console.log(`Parsed path: ${pathExpression}`);
        const pathQueryResult = await engine.executePathQuery(parsedPath.data);

        if (pathQueryResult.success) {
            console.log(`People and the locations they work in (via path ${pathExpression}):`);
            pathQueryResult.data.items.forEach(i => {
                console.log(`  ${i.subject} works in ${i.object}`);
            });
        }
    }
}

/**
 * Demonstrate predicate discovery features
 */
async function discoverPredicates(engine: PredicateKnowledgeEngine) {
    console.log("\n--- Discovering Predicates ---");

    // Discover potential predicates
    const discoveryParams: PredicateDiscoveryParams = {
        sourceType: 'ex:Person',
        targetType: 'ex:Location',
        maxPathLength: 3,
        minFrequency: 1,
        minConfidence: 0.5
    };

    const discoveryResult = await engine.discoverPotentialPredicates(discoveryParams);
    if (discoveryResult.success) {
        console.log("Discovered potential predicates between Person and Location:");
        discoveryResult.data.forEach(p => {
            console.log(`  ${p.label} (${p.uri})`);
        });
    }

    // Generate paths between Person and Country
    const pathsResult = await engine.generatePaths('ex:Person', 'ex:Country', 3);
    if (pathsResult.success) {
        console.log("Generated paths from Person to Country:");
        pathsResult.data.forEach(path => {
            console.log(`  ${path.path.join(' -> ')}`);
        });
    }

    // Find missing predicates based on constraints
    const missingResult = await engine.findMissingPredicates('graph1');
    if (missingResult.success) {
        console.log("Missing predicates based on constraints:");
        missingResult.data.forEach(missing => {
            console.log(`  ${missing.subject} should have ${missing.predicate} to ${missing.object}`);
        });
    }

    // Suggest constraint candidates
    const suggestionsResult = await engine.suggestConstraintCandidates(0.7);
    if (suggestionsResult.success) {
        console.log("Suggested constraint candidates:");
        suggestionsResult.data.forEach(suggestion => {
            console.log(`  ${suggestion.source} -> ${suggestion.target} (confidence: ${suggestion.confidence.toFixed(2)})`);
        });
    }
}

/**
 * Evaluate the consistency of the knowledge graph
 */
async function evaluateKnowledgeGraph(engine: PredicateKnowledgeEngine) {
    console.log("\n--- Evaluating Knowledge Graph ---");

    // Get all predicate instances
    const allInstances = await engine.queryPredicateInstances({});
    if (!allInstances.success) {
        console.warn("Failed to retrieve instances for evaluation");
        return;
    }

    // Evaluate accuracy against gold standard facts
    const accuracyResult = await engine.evaluateAccuracy(allInstances.data.items, 'graph1');
    if (accuracyResult.success) {
        console.log("Knowledge graph evaluation results:");
        console.log(`  Accuracy (F1 Score): ${accuracyResult.data.accuracy.toFixed(2)}`);
        console.log(`  Consistency Score: ${accuracyResult.data.consistency.toFixed(2)}`);
        console.log(`  Violation Ratio: ${accuracyResult.data.violationRatio.toFixed(2)}`);
        console.log(`  Overall Confidence: ${accuracyResult.data.confidenceScore.toFixed(2)}`);
    }

    // Complete missing predicates
    const completionResult = await engine.completeMissingPredicates('graph1', 0.8);
    if (completionResult.success) {
        console.log("Suggested predicate completions:");
        completionResult.data.forEach(completion => {
            console.log(`  ${completion.subject} ${completion.predicate} ${completion.object} (confidence: ${completion.confidence.toFixed(2)})`);
        });
    }
}

// Run the example
main().catch(console.error);
