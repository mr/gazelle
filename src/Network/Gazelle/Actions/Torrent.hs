module Network.Gazelle.Actions.Torrent (
    getTorrent,
    getTorrentAndGroup,
    getTorrentGroup,
    getTorrentGroupAndChildren
) where

import Network.Gazelle.Types
import Network.Gazelle.Types.Torrent
import Network.Gazelle.Routes

getTorrentAndGroup :: Monad m => TorrentID -> GazelleT m (Torrent, TorrentGroup)
getTorrentAndGroup t = do
    tag <- receiveRoute $ torrentRoute t
    return (tagTorrent tag, tagGroup tag)

getTorrent :: Monad m => TorrentID -> GazelleT m Torrent
getTorrent t = do
    (torrent, _) <- getTorrentAndGroup t
    return torrent

getTorrentGroupAndChildren :: Monad m => TorrentGroupID -> GazelleT m (TorrentGroup, [Torrent])
getTorrentGroupAndChildren t = do
    tgc <- receiveRoute $ torrentGroupRoute t
    return (tgcGroup tgc, tgcChildren tgc)

getTorrentGroup :: Monad m => TorrentGroupID -> GazelleT m TorrentGroup
getTorrentGroup t = do
    (tg, _) <- getTorrentGroupAndChildren t
    return tg
