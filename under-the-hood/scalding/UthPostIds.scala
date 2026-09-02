package com.twitter.visibility.under_the_hood

object UthPostIds {
  val MaxPostIdsPerLabel: Int = 1000

  final case class Summary(carried: Long, removed: Long, postIds: Seq[Long])

  def summarize(rows: Iterable[(Long, Long)]): Summary = {
    val byPostId = rows.groupBy(_._1)
    val postIds = byPostId.keys.toSeq.sorted
    Summary(
      carried = postIds.size.toLong,
      removed = byPostId.values.count(_.exists(_._2 > 0L)).toLong,
      postIds = postIds
    )
  }

  def newestDistinct(postIds: Iterable[Long], limit: Int): Seq[Long] = {
    require(limit > 0, s"limit must be > 0; got $limit")
    postIds.toSet.toSeq.sorted.takeRight(limit)
  }
}
