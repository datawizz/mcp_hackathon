// Define predicates for an organization domain
await defineOrganizationKnowledgeGraph(engine);

// Define gold standard constraints (e.g., worksAt → worksIn)
await defineConstraints(engine);

// Perform various queries on the knowledge graph
await performQueries(engine);

// Discover new predicates using various techniques
await discoverPredicates(engine);

// Evaluate consistency against gold standard constraints
await evaluateKnowledgeGraph(engine);
