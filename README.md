## Configuration

1. Configure your port id and Database Coneection string accordingly
   Example --
   PORT = 3030
   DB_CNN = "Your DB Conection String"

2. App is starting From - app.js

3. Technical design document: https://github.com/AIQUANT-Tech/CardanoRnR/blob/main/DesignDocs/CardanoRnR_TechDesign.pdf
4. UI Design Document: https://www.figma.com/design/FACv8nNkArAruZd9gsdvpR/RnR?node-id=18-320&t=3BpMSOHQf3Sv40fG-1

## Installation

### 1. Clone the Repository

git clone https://github.com/AIQUANT-Tech/CardanoRnR.git

### 2. Setup the Backend

Change directory to the backend folder and install dependencies:

cd CardanoRnR/server
npm install

#### Configure the Environment Variables

Inside the `server` folder, create a `.env` file and add your configuration details. Example:

PORT=3000
DB_CNN="your-database-connection-string"
SCRIPT_CBOR="your-script-cbor"
RECIPIENT_ADD="recipient-address"
AMMOUNT_ADA=1000
AMMOUNT=10
MNEMONIC="your-mnemonic-phrase"
BLOCKFROST_KEY="your-blockfrost-key"
PLUTUS_SCRIPT_PATH="path/to/plutus/script"
REDIS_HOST=localhost
REDIS_PORT=6379
REDIS_MAX_RETRIES_PER_REQUEST=null

_Note: Replace the placeholder values with your own details._

### 3. Setup the Frontend

Change directory to the frontend folder and install dependencies:

cd ../front-end-ui
npm install

#### Configure the API Base URL

Inside `front-end-ui/src`, open `config.js` and set your API base URL:

const API_BASE_URL = "http://localhost:8080/"; // change if needed
export default API_BASE_URL;

## Running the Application

### Start the Backend

From the `server` directory, run:

npm start

### Start the Frontend

From the `front-end-ui` directory, run:

npm start

## Additional Notes

- Ensure the environment variables in the `.env` file are set up correctly.

## Smart Contract

This repository includes a Plutus smart contract (e.g., in the file `Review/RnR.hs`). To generate the CBOR hex file of the smart contract, follow these steps:

1. **Navigate to the Smart Contract Directory**

   Go to the directory containing your smart contract file. For example:

   cd path/to/smart-contract-directory

2. **Clean, Update, Build, and Open the REPL**

   Run the following commands in your terminal:

   cabal clean
   cabal update
   cabal build
   cabal repl

3. **Generate the CBOR Hex File**

   Once inside the Cabal REPL, run the command:

   writeScript

   This will generate a file named `Review/RnR.plutus` containing the CBOR hex representation of your compiled Plutus script.

_Note: Ensure that you have all the necessary dependencies and proper Cabal configuration before running these commands._
