%% Parse and separate out relevant parts of the TPD
-record(tpd,{
	 name="",
	 path="",
	 closure=[],
	 ref_projects=[],
	 files=[],
	 active_config="",
	 configs=[]
	}).


