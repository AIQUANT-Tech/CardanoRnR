import React from "react";

interface Product {
  id: number;
  name: string;
  price: number;
  image: string;
}

interface CardProps {
  item: Product;
  onAdd: (item: Product) => void;
}

const Card: React.FC<CardProps> = ({ item, onAdd }) => {
  return (
    <div className="bg-white shadow-lg rounded-xl p-4 w-60 text-center">
      <img
        src={item.image}
        alt={item.name}
        className="h-32 w-full object-cover rounded-md mb-3"
      />
      <h2 className="text-lg font-semibold">{item.name}</h2>
      <p className="text-gray-600 mb-2">₹{item.price}</p>
      <button
        onClick={() => onAdd(item)}
        className="bg-blue-600 text-white py-1 px-3 rounded-lg hover:bg-blue-700"
      >
        Add to Cart
      </button>
    </div>
  );
};

export default Card;
