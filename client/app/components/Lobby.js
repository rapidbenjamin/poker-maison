import React from 'react';

const Lobby = ({ lobby }) => {
  console.log(lobby)
  return (
    <table className="table">
      <thead>
        <tr>
          <th><abbr title="Position">Pos</abbr></th>
          <th>Team</th>
          <th><abbr title="Played">Pld</abbr></th>
          <th><abbr title="Won">W</abbr></th>
          <th><abbr title="Drawn">D</abbr></th>
          <th><abbr title="Lost">L</abbr></th>
          <th><abbr title="Goals for">GF</abbr></th>
          <th><abbr title="Goals against">GA</abbr></th>
          <th><abbr title="Goal difference">GD</abbr></th>
          <th><abbr title="Points">Pts</abbr></th>
          <th>Qualification or relegation</th>
        </tr>
      </thead>
      <tfoot>
        <tr>
          <th><abbr title="Position">Pos</abbr></th>
          <th>Team</th>
          <th><abbr title="Played">Pld</abbr></th>
          <th><abbr title="Won">W</abbr></th>
          <th><abbr title="Drawn">D</abbr></th>
          <th><abbr title="Lost">L</abbr></th>
          <th><abbr title="Goals for">GF</abbr></th>
          <th><abbr title="Goals against">GA</abbr></th>
          <th><abbr title="Goal difference">GD</abbr></th>
          <th><abbr title="Points">Pts</abbr></th>
          <th>Qualification or relegation</th>
        </tr>
      </tfoot>
      <tbody>
        <tr>
          <th>1</th>
          <td><a title="Leicester City F.C.">Leicester City</a> <strong>(C)</strong>
          </td>
          <td>38</td>
          <td>23</td>
          <td>12</td>
          <td>3</td>
          <td>68</td>
          <td>36</td>
          <td>+32</td>
          <td>81</td>
          <td>Qualification for the <a title="2016–17 UEFA Champions League">Champions League group stage</a></td>
        </tr>
      </tbody>
    </table>);
}

export default Lobby;
