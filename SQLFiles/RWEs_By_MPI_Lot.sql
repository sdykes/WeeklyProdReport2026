SELECT 
	se.SeasonDesc AS Season
	,gbml.GraderBatchMPILotID
	,gb.GraderBatchID
	,gbml.MPILotNo
	,fa.FarmCode AS RPIN
	,fa.FarmName AS Orchard
	,sb.SubdivisionCode AS [Production site]
	,gb.PackDate
	,gb.HarvestDate
	,ISNULL(rmlpl.RWEs,0) + ISNULL(rmlpr.RWEs,0) AS RWEs
FROM ma_Grader_Batch_MPI_LotT AS gbml
LEFT JOIN
	ma_Grader_BatchT AS gb
ON gb.GraderBatchID = gbml.GraderBatchID
LEFT JOIN
	sw_FarmT AS fa
ON fa.FarmID = gb.FarmID
LEFT JOIN
	sw_SubdivisionT AS sb
ON sb.SubdivisionID = gb.SubdivisionID
LEFT JOIN
	sw_SeasonT AS se
ON se.SeasonID = gb.SeasonID
LEFT JOIN
	shiny_RWEs_By_MPI_Lot_PalletV AS rmlpl
ON rmlpl.GraderBatchMPILotID = gbml.GraderBatchMPILotID
LEFT JOIN
	shiny_RWEs_By_MPI_Lot_PresizeV AS rmlpr
ON rmlpr.GraderBatchMPILotID = gbml.GraderBatchMPILotID


