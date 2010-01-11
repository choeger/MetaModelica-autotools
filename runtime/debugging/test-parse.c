extern int rmldb_load_db(char* programdb_file)
{
	char line[RML_DEBUG_MAX_STRING];  /* declare a char array */
	struct rml_var_db *rml_var_db_node;
	struct rml_type_db *rml_type_db_node;
	struct rml_con_db *rml_con_db_node;
	struct rml_relation_db *rml_relation_db_node;

	fprintf(stderr, "loading program database from: %s\n", programdb_file);

	aarmldbin = fopen(programdb_file, "r"); 
	/* open a text file for reading */

	if(aarmldbin==NULL) 
	{
		fprintf(stderr, "Error: can't open file %s.\n", programdb_file);
		/* fclose(file); DON'T PASS A NULL POINTER TO fclose !! */
		rml_exit(2);
	}
	else 
	{
		if (!aarmldbparse())
		  fprintf(stderr, "parsing of program database for file %s failed!\n", programdb_file); 
		fclose(aarmldbin);
		return 0;
	}
}

