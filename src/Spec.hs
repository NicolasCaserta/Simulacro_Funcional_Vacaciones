module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de Ejemplo" $ do
    it "Funciona Joya" $ do
      stress (cambiarStress 1 ana) `shouldBe` 21