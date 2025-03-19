<h1>Natural Language Processing + Search</h1>
<p><b>Description: </b>This program uses prolog to represnt a model made up of different blocks. Using the included natural language processor these blocks can be identified by color, shape, relative location, and name. The blocks can also be moved to any loaction using an AI search algorithm. </p>
<p><b>To Run: </b>Open swipl and run [loadall]. to load all the programs. Then use np([desription,words], B) to generate a block that matches your description. command([put,block,description,on,second,block,decription]) can be used to move a single block to a valid location. command([achieve,block,description,on,second,block,description]) will move the 1st block to a different location using AI to make the move possible. </p>

<h2>Project Elements: </h2>
<ul>
  <li>
    <h4>World Model:</h4>
    <p><b>Description: </b>Contains predicates that describe the blocks and how they are placed in the world. Information includes: name, shape, color, location and which blocks are beside or above each other. </p>
    <p><b>Location: </b>world_model.pl</p>
  </li>
  <li>
    <h4>Lexicon: </h4>
    <p><b>Description: </b>Takes the information from world model and turns it into its cooresponding part of speech. This allows the natural language processor to use that information. </p>
    <p><b>Location: </b>lexicon.pl</p>
  </li>
  
  <li>
    <h4>Natural Language Processor: </h4>
    <p><b>Description: </b>Allows the world to be interacted with using natural human language. The predicate NP allows the user to find blocks based off of their characteristics. Command allows the user to command the AI to modify the world. </p>
    <p><b>Location: </b>np.pl</p>
  </li>
  <li>
    <h4>Iterative Deepening Search (with cycle prevention): </h4>
    <p><b>Description: </b>The idcp_plan() predicate allows the AI model to find a plan for moving a block to a given spot. </p>
    <p><b>Location: </b>search.pl</p>
  </li>
  <li>
    <h4>Moving Blocks: </h4>
    <p><b>Description: </b>Using the plan provided by the search algorithm, the predicate execute_plan() can move the blocks into the correct location. Use achieve_on() to make this all in one line. </p>
    <p><b>Location: </b>search.pl</p>
  </li>  
  <li>
    <h4>Loadall: </h4>
    <p><b>Description: </b>Loads all of the programs into swipl using just one call. [loadall]. </p>
    <p><b>Location: </b>loadall.pl</p>
  </li>
</ul>
