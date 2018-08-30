import React from 'react';

import Card from './Card';

const Board = ({ cards }) => (
  <div className="board-cards">
    <div className='board-cards-container'>
      {cards.map(card => {
        console.log(card.toJS())
        const rank = card.get('rank')
        const suit = card.get('suit')

        return (<Card
          key={rank + suit}
          rank={rank}
          suit={suit}
        />)
      })}
    </div>
  </div>);

export default Board;