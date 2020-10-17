**Aggregation Procedure**

***New cases & deaths***
1. Sum up province data into a country total
	- check each row if province/state is **NA**
	    - if yes, treat it as a total
    	- if no, add all rows for that country/region for the entire series
2. Incorporate 'ISO' based on 3-digit alpha codes
3. Apply `melt` function to new DataFrame and `pivot_table`, with ISO and activity set as index
	
***Google Activity***
1. Retain a subset of the original data containing national stats without subregional breakdown
2. Rename activity types for conventional purposes (i.e. retail and recreation as retail)
3. Apply `melt` function to new DataFrame and `pivot_table`, with ISO and activity set as index
4. Replace 2-digit country code with 3-digit ISO convention

***Interventions***
1. Create a new DataFrame with `MultiIndex` - Category and ISO
2. Ensure ISO code conforms to 3-digit convention
