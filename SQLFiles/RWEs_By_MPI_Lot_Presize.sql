SELECT 
	se.SeasonDesc AS Season
	,bd.PresizeOutputFromGraderBatchMPILotID AS GraderBatchMPILotID
	,SUM(CAST(bd.TotalWeight / ctr.KGsPerRWE AS numeric(10, 2))) AS RWEs
FROM  dbo.ma_Bin_DeliveryT AS bd 
INNER JOIN
    dbo.ma_Grader_BatchT AS gb 
ON gb.GraderBatchID = bd.PresizeOutputFromGraderBatchID 
INNER JOIN
    dbo.sw_ProductT AS pr 
ON pr.ProductID = bd.PresizeProductID 
INNER JOIN
    dbo.sw_GradeT AS gt 
ON gt.GradeID = pr.GradeID 
LEFT JOIN
	sw_SeasonT AS se
ON se.SeasonID = bd.SeasonID
CROSS JOIN
    dbo.sw_ControlT AS ctr
WHERE (bd.PresizeFlag = 1) 
AND (gt.JuiceFlag = 0)
GROUP BY
	se.SeasonDesc 
	,bd.PresizeOutputFromGraderBatchMPILotID 


