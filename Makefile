setup:
	(cd ./lib/postgrest && stack setup)
	(cd ./lib/postgrest && stack build)

	psql -c "CREATE DATABASE postgrest_demo;"
