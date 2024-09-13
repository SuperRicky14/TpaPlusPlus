package net.superricky.tpaplusplus.database

import com.google.common.cache.Cache
import com.google.common.cache.CacheBuilder
import java.util.UUID

object DatabaseCacheService {
    val playerKeys: Cache<UUID, Int> = CacheBuilder.newBuilder().build()
}
