SELECT 
c.campaign_id, 
c.month, 
p.product_name, 
cs.impressions, 
cs.clicks,
cs.conversion_rate,
c.campaign_type,
cs.CPL,
cs.EPL

FROM campaigns c
LEFT JOIN campaign_statistics cs ON
c.stats_id =  cs.stats_id
LEFT JOIN products p ON
c.product_id = p.product_id

 
