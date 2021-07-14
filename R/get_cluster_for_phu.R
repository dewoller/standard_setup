get_cluster_for_phu <- function(which_phu, ClusterSites, AllClusters) {
  ClusterSites %>%
  filter(ExposureSiteRecordOwner == which_phu & (Status == "Active" | Status == "New" | Status == "Escalated")) %>%
    select(ClusterID, ExposureCaseNumber, ClusterName, Status, ClusterType, DeclaredDate, ClusterClosedDate, ExposureSiteRecordOwner) %>%
    left_join(AllClusters %>%
              select(RecordID, ClusterID), by = "ClusterID")

  }
