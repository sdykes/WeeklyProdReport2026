SELECT 
	pd.PalletDetailID
	,pd.PalletID
	,se.SeasonDesc AS Season
	,pd.ProductID
	,pr.ProductDesc AS [SKU description]
	,PackTypeDesc AS [Pack type]
	,PalletTypeDesc AS [Pallet type]
	,pd.NoOfUnits
	,pr.TubesPerCarton
	,tt.RTEConversion
	,tt.PresizeAvgTubeWeight
	,pr.StandardPackedNetFruitWeight
	,CAST(CASE 
			WHEN se.RWEUnitOfMeasureFlag = 1 THEN NULL 
			ELSE CASE 
					WHEN ebd.ExportBinKGWeight IS NOT NULL THEN (ebd.ExportBinKGWeight / tt.PresizeAvgTubeWeight) * tt.RTEConversion 
					ELSE pd.NoOfUnits * pr.TubesPerCarton * tt.RTEConversion 
				END
		  END AS numeric(10, 2)) AS RTEs
	,CAST(CASE 
			WHEN ebd.ExportBinKGWeight IS NOT NULL THEN ebd.ExportBinKGWeight 
			ELSE ISNULL(pd.TotalCartonWeightNet, 0) + (CASE 
															WHEN pd.TotalCartonWeightNet IS NOT NULL THEN ISNULL(pd.NoOfUnitsUnweighed, 0) 
															ELSE pd.NoOfUnits 
														  END * pr.StandardPackedNetFruitWeight) 
			  END / ctr.KGsPerRWE AS numeric(10, 2)) AS RWEs
	,pd.GraderBatchID
	,ctp.CompanyName AS [Packing site]
	,ctg.CompanyName AS Grower
	,fa.FarmName AS Orchard
FROM ma_Pallet_DetailT AS pd
LEFT JOIN
	ma_PalletT AS pa
ON pa.PalletID = pd.PalletID
LEFT JOIN
	(
	SELECT 
		PalletDetailID
		,SUM(KGWeight) AS ExportBinKGWeight
    FROM dbo.ma_Export_Bin_DetailT
    GROUP BY PalletDetailID
	) AS ebd 
ON ebd.PalletDetailID = pd.PalletDetailID
LEFT JOIN
	sw_Pallet_TypeT pty
ON pty.PalletTypeID = pa.PalletTypeID
LEFT JOIN
	sw_CompanyT AS ctp
ON ctp.CompanyID = pd.PackerCompanyID
LEFT JOIN
	sw_SeasonT AS se
ON se.SeasonID = pa.SeasonID
LEFT JOIN
	sw_ProductT AS pr
ON pr.ProductID = pd.ProductID
LEFT JOIN
	sw_Pack_TypeT AS pt
ON pt.PackTypeID = pr.PackTypeID
LEFT JOIN
	sw_Tube_TypeT AS tt
ON tt.TubeTypeID = pr.TubeTypeID
LEFT JOIN
	ma_Grader_BatchT AS gb
ON gb.GraderBatchID = pd.GraderBatchID
LEFT JOIN
	sw_FarmT AS fa
ON fa.FarmID = gb.FarmID
LEFT JOIN
	sw_CompanyT AS ctg
ON ctg.CompanyID = fa.GrowerCompanyID
CROSS JOIN
    dbo.sw_ControlT AS ctr





