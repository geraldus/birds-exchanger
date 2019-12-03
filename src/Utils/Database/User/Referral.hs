module Utils.Database.User.Referral where

import           Import.NoFoundation hiding ( on, (==.) )

import           Database.Esqueleto


getCreateRefTokenDB ::
    MonadIO m => UserId -> Text -> SqlPersistT m (Entity Referrer)
getCreateRefTokenDB u token = do
    let absent = Referrer u token
    upsertBy (UniqueReferrer u) absent []


getReferralsOfDB ::
       MonadIO m
    => [ReferrerId]
    -> SqlPersistT m [(Entity Referral, Entity User, Entity Referrer)]
getReferralsOfDB refs = select . from $
    \(rel `InnerJoin` u `InnerJoin` ref) -> do
        on (u ^. UserId ==. ref ^. ReferrerUser)
        on (rel ^. ReferralUser ==. u ^. UserId)
        where_ ((rel ^. ReferralReferrer) `in_` valList refs)
        return (rel, u, ref)
