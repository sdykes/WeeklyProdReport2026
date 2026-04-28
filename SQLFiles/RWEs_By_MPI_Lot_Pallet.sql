SELECT 
    ct.Season
	,ct.GraderBatchMPILotID
	,SUM(CAST(CASE 
			WHEN ebd.ExportBinKGWeight IS NOT NULL THEN ebd.ExportBinKGWeight 
			ELSE ISNULL(pd.TotalCartonWeightNet, 0) + (CASE 
														 WHEN pd.TotalCartonWeightNet IS NOT NULL THEN ISNULL(pd.NoOfUnitsUnweighed, 0) 
														 ELSE ct.NoOfUnits 
													   END * pr.StandardPackedNetFruitWeight) 
			END / ctr.KGsPerRWE AS numeric(10, 2))) AS RWEs 
FROM ma_Pallet_DetailT AS pd
LEFT JOIN
	(
	SELECT
		se.SeasonDesc AS Season
		,co.GraderBatchMPILotID
		,co.PalletDetailID
		,co.CartonExistsFlag
		,COUNT(co.CartonNo) AS NoOfUnits
	FROM ma_CartonT AS co
	LEFT JOIN
		sw_SeasonT AS se
	ON se.SeasonID = co.SeasonID
	WHERE co.PalletDetailID IS NOT NULL
	GROUP BY 
		se.SeasonDesc
		,co.GraderBatchMPILotID
		,co.PalletDetailID
		,co.CartonExistsFlag
	) AS ct
ON ct.PalletDetailID = pd.PalletDetailID
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
	sw_ProductT AS pr
ON pr.ProductID = pd.ProductID
CROSS JOIN
   dbo.sw_ControlT AS ctr
WHERE ct.CartonExistsFlag = 1
/*AND ct.Season = 2026*/
AND CAST(CASE 
			WHEN ebd.ExportBinKGWeight IS NOT NULL THEN ebd.ExportBinKGWeight 
			ELSE ISNULL(pd.TotalCartonWeightNet, 0) + (CASE 
														 WHEN pd.TotalCartonWeightNet IS NOT NULL THEN ISNULL(pd.NoOfUnitsUnweighed, 0) 
														 ELSE ct.NoOfUnits 
													   END * pr.StandardPackedNetFruitWeight) 
			END / ctr.KGsPerRWE AS numeric(10, 2)) IS NOT NULL
GROUP BY
	ct.Season
	,ct.GraderBatchMPILotID






