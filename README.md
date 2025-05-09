## Configuration

1. Technical design document: https://github.com/AIQUANT-Tech/CardanoRnR/blob/main/DesignDocs/CardanoRnR_TechDesign.pdf
2. UI Design Document: https://www.figma.com/design/FACv8nNkArAruZd9gsdvpR/RnR?node-id=18-320&t=3BpMSOHQf3Sv40fG-1
3. Used API Document: https://github.com/AIQUANT-Tech/CardanoRnR/tree/main/server/Published_API_Design_Doc
4. CardanoRnR Test Report for Smart Contract TestScenario : https://github.com/AIQUANT-Tech/CardanoRnR/tree/main/TestReport/RnRSmartContractTestScenario
5. CardanoRnR Test Cases Report for Overall : https://github.com/AIQUANT-Tech/CardanoRnR/tree/main/TestReport/TestCaseReportCardanoRnR

6. Deployment URLs

   Business User
   The deployment URL for Business users is designed to provide access to all business-related functionalities and features. You can access it here:

     http://51.21.61.199/     

   End User
   The deployment URL for End users is tailored to offer a seamless experience for our end customers. You can access it here:

     http://51.21.61.199/user/X 


7. Test Script Address : addr_test1wzeez0kg9flu96ekkxwq3kur9jxa847pawansrr259u7cagrscnpu     **It be Differ for Each Entity**

      Test Script Address in Cardano Scan : https://preprod.cardanoscan.io/address/70b3913ec82a7fc2eb36b19c08db832c8dd3d7c1ebbb380c6aa179ec75
      
8. Test Transaction hash of one Succesful Review Redeemtion and Reputation Score Generation --   b04bb963145741a0f08676d8997e1b04d483220e1a03dc46cdd199b6a18a5844

      Cardano Scan Link in Prepod of Test Transaction hash -- https://preprod.cardanoscan.io/transaction/b04bb963145741a0f08676d8997e1b04d483220e1a03dc46cdd199b6a18a5844 
   
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

SCRIPT_CBOR="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" **//Hardcoded** **Now its Parameterized So, the CBOR be changed for each Bussiness User.**

AMMOUNT_ADA=850000  //calculated Minimum fees for this Transaction in lovelace  **Hardcoded**

AMMOUNT=0.85  //calculated Minimum fees for this Transaction in ada   **Hardcoded**

MNEMONIC="your-mnemonic-phrase"

BLOCKFROST_KEY="your-blockfrost-key"

PLUTUS_SCRIPT_PATH="/home/aiquant/Desktop/git/CardanoRnR/CardanoRnR/SmartContract/Review/RnR.plutus" **//Hardcoded**

REDIS_HOST=localhost

REDIS_PORT=6379

REDIS_MAX_RETRIES_PER_REQUEST=null

_Note: Replace the placeholder values with your own details._

##How to Configure?

**DB_CNN --**

DB_CNN --
 
a. Install MongoDB and MongoDB Compass
 
b. Start MongoDB Server
 
c. Connect to MongoDB Shell
 
d. Create Database DB_CNN
 
e. Create a Collection
 
f. Connect MongoDB Remotely
 
g. The Database DB_CNN provide it in the place of "your-database-connection-string" in DB_CNN (ex. "mongodb+srv://<projectId>:<password>@rnrdb.allz7.mongodb.net/?retryWrites=true&w=majority&appName=RnRDb")

**MNEMONIC --**

Choose a Cardano Wallet

a. Install the Wallet

b. Launch the Wallet & Create a New Wallet or Import an existing wallet

c. Automatic Seed Phrase Generation(12 words or 24 words)

d. Record and Store Your Seed Phrase (Including Storing in a Text File)

e. Confirm Your Seed Phrase

f. Finalize Wallet Setup

g. Give the Seed Phrase provide it in the place of "your-mnemonic-phrase" in the MNEMONIC(ex. "slight magnet cage frost puzzle moon ripple undo toilet orphan crash heavy slight magnet cage frost puzzle moon ripple undo toilet orphan crash heavy")

**BLOCKFROST_KEY --**

a. Visit the Blockfrost Website

b. Register for an Account

c. Verify Your Email

d. Log In to Your Account

e. Create a New Project

f. Retrieve Your API Key

g. Securely Store Your API Key

h. Your API Key be provide it in the place of the "your-blockfrost-key" in BLOCKFROST_KEY(ex. "preprod1234567890abcdef1234567890abcdef")

**## The Full Example of the .env File**

PORT=3000

DB_CNN="mongodb+srv://user:password@cluster0.mongodb.net/mydatabase?retryWrites=true&w=majority"

SCRIPT_CBOR="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" **//Hardcoded** **Now its Parameterized So, the CBOR be changed for each Bussiness User.**

AMMOUNT_ADA=850000  ///calculated Minimum fees for this Transaction in lovelace  **Hardcoded**

AMMOUNT=0.85  //calculated Minimum fees for this Transaction in ada   **Hardcoded**

MNEMONIC="slight magnet cage frost puzzle moon ripple undo toilet orphan crash heavy slight magnet cage frost puzzle moon ripple undo toilet orphan crash heavy"

BLOCKFROST_KEY="preprod1234567890abcdxxxxxef1234567890abcdef"  

PLUTUS_SCRIPT_PATH="SmartContract/Review/RnR.plutus"  **//Hardcoded**

REDIS_HOST=127.0.0.1

REDIS_PORT=6379

REDIS_MAX_RETRIES_PER_REQUEST=null

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

App is starting From - app.js

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

   cabal clean   //Removes previous build artifacts to ensure a clean slate and avoid conflicts.
   
   cabal update  //Refreshes the local package repository index so that dependencies are up-to-date.

   cabal build  //Compiles the smart contract code into an executable form.

   cabal repl   //Launches an interactive environment for testing your smart contract functions.


4. **Generate the CBOR Hex File**

   Once inside the cabal repl, run the command:

   Reputation.RnR> main

   Enter business PubKeyHash (hex):

   avcdfhjhhxxxxxxxxxxxxxxx8687575gdxxxxxxxx                 **Parametarized**

   Enter output filepath (e.g. compiled/ReputationRnR.plutus):

   xxxx/xxxxxxxxxxxx/xxxxxxxxxxxxx/xxxxxxxx/ReputationRnR.plutus
   
   Script written.  //Generate the .plutus file of the smart contract, which is the CBOR hex of the smart contract.

   This will generate a file named `Review/RnR.plutus` containing the CBOR hex representation of your compiled Plutus script.

Then In cli Generate the Smart Contract Address

By using -- 
  cardano-cli address build \
    --payment-script-file RnR.plutus \
    --out-file script.addr --testnet-magic 1

## Smart Contract Test

1. **Unit test cases**
This repository includes a unit test for Plutus smart contract (e.g., in the file `SmartContract/Test/RnRTest.hs`). To run the test file of the smart contract, follow these steps:

   **a. Navigate to the Smart Contract Directory**

   Go to the directory containing your smart contract file. For example:

   cd path/to/smart-contract-directory

   **b. Clean, Update, Build, and Open the REPL**

   Run the following commands in your terminal:

   cabal clean   //Removes previous build artifacts to ensure a clean slate and avoid conflicts.
   
   cabal update  //Refreshes the local package repository index so that dependencies are up-to-date.

   cabal build  //Compiles the smart contract code into an executable form.

   cabal repl   //Launches an interactive environment for testing your smart contract functions.
   
   **c. Run the test file**
   
   In "Review.RnrTest>" write main function.
   

**_Note: Ensure that you have all the necessary dependencies and proper Cabal configuration before running these commands._**
