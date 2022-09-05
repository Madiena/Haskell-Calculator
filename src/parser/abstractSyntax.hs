module AbstractSyntax() where

data BinaryTree a = Node (BinaryTree String) String (BinaryTree String) | Leaf String



