import           Test.Hspec

import qualified Data.Map   as M

import           Trie

-- | Single (k,v) in dictionary: [a=kek]
empty = Trie False "" M.empty
aKek = Trie False "" (M.singleton 'a' (Trie True "kek" M.empty))
bLol = Trie False "" (M.singleton 'b' (Trie True "lol" M.empty))
aKekBlol = Trie False "" (M.fromList [('a', (Trie True "kek" M.empty)), ('b', (Trie True "lol" M.empty))])

main :: IO ()
main = hspec $ parallel $ do
    describe "[Trie] Lookup" $ do
      it "lookup 'a' in []" $ do
        get "a" empty `shouldBe` Nothing
      it "lookup 'a' in [a=kek]" $ do
        get "a" aKek `shouldBe` Just "kek"
      it "lookup 'b' in [a=kek]" $ do
        get "b" aKek `shouldBe` Nothing
      it "lookup 'a' in [a=kek,b=lol]" $ do
        get "a" aKekBlol `shouldBe` Just "kek"
      it "lookup 'b' in [a=kek,b=lol]" $ do
        get "b" aKekBlol `shouldBe` Just "lol"
      it "lookup 'c' in [a=kek,b=lol]" $ do
        get "c" aKekBlol `shouldBe` Nothing
    describe "[Trie] Insertion" $ do
      it "put 'a'='kek' in []" $ do
        put "a" "kek" empty `shouldBe` (Nothing, aKek)
      it "put 'a'='kek' in [a=kek]" $ do
        put "a" "kek" aKek `shouldBe` (Just "kek", aKek)
      it "put 'b'='lol' in []" $ do
        put "b" "lol" empty `shouldBe` (Nothing, bLol)
      it "put 'a'='kek' and 'b'='lol' to []" $ do
        let (_, t) = put "a" "kek" empty
        let (_, t') = put "b" "lol" t
        t' `shouldBe` aKekBlol
      it "put 'a'='kek' to [b=lol]" $ do
        put "a" "kek" bLol `shouldBe` (Nothing, aKekBlol)
    describe "[Trie] Removal" $ do
      it "remove 'a' from [a=kek]" $ do
        remove "a" aKek `shouldBe` (Just "kek", empty)
      it "remove 'b' from [a=kek]" $ do
        remove "b" aKek `shouldBe` (Nothing, aKek)
      it "remove 'a' and 'b' from [a=kek,b=lol]" $ do
        let (a, t') = remove "a" aKekBlol
        a `shouldBe` Just "kek"
        t' `shouldBe` bLol
        let (b, t'') = remove "b" t'
        b `shouldBe` Just "lol"
        t'' `shouldBe` empty
--    describe "[Trie] Fold" $ do
--      it "concat everything from [a=kek,b=lol]" $ do
--        foldd (:) [] aKekBlol `shouldBe` [("akek", "blol")]
