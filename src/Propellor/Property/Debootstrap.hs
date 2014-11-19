module Propellor.Property.Debootstrap (
	Url,
	debootstrapped,
	installed,
	debootstrapPath,
) where

import Propellor
import qualified Propellor.Property.Apt as Apt
import Utility.Path
import Utility.SafeCommand
import Utility.FileMode

import Data.List
import Data.Char
import Control.Exception
import System.Posix.Directory

type Url = String

-- | Builds a chroot in the given directory using debootstrap.
--
-- The System can be any OS and architecture that debootstrap
-- and the kernel support.
debootstrapped :: FilePath -> System -> [CommandParam] -> Property
debootstrapped target system@(System _ arch) extraparams = 
	check (unpopulated target) prop
		`requires` unrevertable installed
  where
	unpopulated d = null <$> catchDefaultIO [] (dirContents d)

	prop = property ("debootstrapped " ++ target) $ liftIO $ do
		createDirectoryIfMissing True target
		let suite = case extractSuite system of
			Nothing -> error $ "don't know how to debootstrap " ++ show system
			Just s -> s
		let params = extraparams ++
			[ Param suite
			, Param target
			, Param $ "--arch=" ++ arch
			]
		cmd <- fromMaybe "debootstrap" <$> debootstrapPath
		ifM (boolSystem cmd params)
			( do
				fixForeignDev target
				return MadeChange
			, return FailedChange
			)

extractSuite :: System -> Maybe String
extractSuite (System (Debian s) _) = Just $ Apt.showSuite s
extractSuite (System (Ubuntu r) _) = Just r

-- | Ensures debootstrap is installed.
--
-- When necessary, falls back to installing debootstrap from source.
-- Note that installation from source is done by downloading the tarball
-- from a Debian mirror, with no cryptographic verification.
installed :: RevertableProperty
installed = RevertableProperty install remove
  where
	install = withOS "debootstrap installed" $ \o -> 
		ifM (liftIO $ isJust <$> debootstrapPath)
			( return NoChange
			, ensureProperty (installon o)
			)

	installon (Just (System (Debian _) _)) = aptinstall
	installon (Just (System (Ubuntu _) _)) = aptinstall
	installon _ = sourceInstall

	remove = withOS "debootstrap removed" $ ensureProperty . removefrom
	removefrom (Just (System (Debian _) _)) = aptremove
	removefrom (Just (System (Ubuntu _) _)) = aptremove
	removefrom _ = sourceRemove
			
	aptinstall = Apt.installed ["debootstrap"]
	aptremove = Apt.removed ["debootstrap"]

sourceInstall :: Property
sourceInstall = property "debootstrap installed from source"
	(liftIO sourceInstall')

sourceInstall' :: IO Result
sourceInstall' = withTmpDir "debootstrap" $ \tmpd -> do
	let indexfile = tmpd </> "index.html"
	unlessM (download baseurl indexfile) $
		error $ "Failed to download " ++ baseurl
	urls <- reverse . sort -- highest version first
		. filter ("debootstrap_" `isInfixOf`)
		. filter (".tar." `isInfixOf`)
		. extractUrls baseurl <$>
		readFileStrictAnyEncoding indexfile
	nukeFile indexfile

	tarfile <- case urls of
		(tarurl:_) -> do
			let f = tmpd </> takeFileName tarurl
			unlessM (download tarurl f) $
				error $ "Failed to download " ++ tarurl
			return f
		_ -> error $ "Failed to find any debootstrap tarballs listed on " ++ baseurl

	createDirectoryIfMissing True localInstallDir
	bracket getWorkingDirectory changeWorkingDirectory $ \_ -> do
		changeWorkingDirectory localInstallDir
		unlessM (boolSystem "tar" [Param "xf", File tarfile]) $
			error "Failed to extract debootstrap tar file"
		nukeFile tarfile
		l <- dirContents "."
		case l of
			(subdir:[]) -> do
				changeWorkingDirectory subdir
				makeDevicesTarball
				makeWrapperScript (localInstallDir </> subdir)
				return MadeChange
			_ -> error "debootstrap tar file did not contain exactly one dirctory"

sourceRemove :: Property
sourceRemove = property "debootstrap not installed from source" $ liftIO $
	ifM (doesDirectoryExist sourceInstallDir)
		( do
			removeDirectoryRecursive sourceInstallDir
			return MadeChange
		, return NoChange
		)

sourceInstallDir :: FilePath
sourceInstallDir = "/usr/local/propellor/debootstrap"

wrapperScript :: FilePath
wrapperScript = sourceInstallDir </> "debootstrap.wrapper"

-- | Finds debootstrap in PATH, but fall back to looking for the
-- wrapper script that is installed, outside the PATH, when debootstrap
-- is installed from source.
debootstrapPath :: IO (Maybe FilePath)
debootstrapPath = getM searchPath
	[ "debootstrap"
	, wrapperScript
	]

makeWrapperScript :: FilePath -> IO ()
makeWrapperScript dir = do
	createDirectoryIfMissing True (takeDirectory wrapperScript)
	writeFile wrapperScript $ unlines
		[ "#!/bin/sh"
		, "set -e"
		, "DEBOOTSTRAP_DIR=" ++ dir
		, "export DEBOOTSTRAP_DIR"
		, dir </> "debootstrap" ++ " \"$@\""
		]
	modifyFileMode wrapperScript (addModes $ readModes ++ executeModes)

-- Work around for http://bugs.debian.org/770217
makeDevicesTarball :: IO ()
makeDevicesTarball = do
	-- TODO append to tarball; avoid writing to /dev
	writeFile foreignDevFlag "1"
	ok <- boolSystem "sh" [Param "-c", Param tarcmd]
	nukeFile foreignDevFlag
	unless ok $
		error "Failed to tar up /dev to generate devices.tar.gz"
  where
	tarcmd = "(cd / && tar cf - dev) | gzip > devices.tar.gz"

fixForeignDev :: FilePath -> IO ()
fixForeignDev target = whenM (doesFileExist (target ++ foreignDevFlag)) $ 
	void $ boolSystem "chroot"
		[ File target
		, Param "sh"
		, Param "-c"
		, Param $ intercalate " && "
			[ "rm -rf /dev"
			, "mkdir /dev"
			, "cd /dev"
			, "/sbin/MAKEDEV std ptmx fd consoleonly"
			]
		]

foreignDevFlag :: FilePath
foreignDevFlag = "/dev/.propellor-foreign-dev"

localInstallDir :: FilePath
localInstallDir = "/usr/local/debootstrap"

-- This http server directory listing is relied on to be fairly sane,
-- which is one reason why it's using a specific server and not a
-- round-robin address.
baseurl :: Url
baseurl = "http://ftp.debian.org/debian/pool/main/d/debootstrap/"

download :: Url -> FilePath -> IO Bool
download url dest = anyM id
	[ boolSystem "curl" [Param "-o", File dest, Param url]
	, boolSystem "wget" [Param "-O", File dest, Param url]
	]

-- Pretty hackish, but I don't want to pull in a whole html parser
-- or parsec dependency just for this.
--
-- To simplify parsing, lower case everything. This is ok because
-- the filenames are all lower-case anyway.
extractUrls :: Url -> String -> [Url]
extractUrls base = collect [] . map toLower
  where
	collect l [] = l
	collect l ('h':'r':'e':'f':'=':r) = case r of
		('"':r') -> findend l r'
		_ -> findend l r
	collect l (_:cs) = collect l cs

	findend l s = 
		let (u, r) = break (== '"') s
		    u' = if "http" `isPrefixOf` u
		    	then u
			else base </> u
		in collect (u':l) r
