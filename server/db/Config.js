import mongoose from "mongoose";
import dotenv from "dotenv";
import crypto from "crypto";

dotenv.config();

const algorithm = "aes-256-gcm";

function decryptDBConnectionString() {
  const encryptedString = process.env.DB_CNN;
  const encryptionKey = process.env.ENCRYPTION_KEY;

  if (!encryptedString) {
    throw new Error("DB_CNN environment variable is required");
  }

  if (!encryptionKey) {
    throw new Error("ENCRYPTION_KEY environment variable is required");
  }

  try {
    const key = Buffer.from(encryptionKey, "base64");
    const parts = encryptedString.split(":");

    if (parts.length !== 3) {
      throw new Error(
        "Invalid encrypted DB connection string format. Expected: iv:authTag:encryptedData"
      );
    }

    const iv = Buffer.from(parts[0], "hex");
    const authTag = Buffer.from(parts[1], "hex");
    const encryptedText = parts[2];

    const decipher = crypto.createDecipheriv(algorithm, key, iv);
    decipher.setAuthTag(authTag);

    let decrypted = decipher.update(encryptedText, "hex", "utf8");
    decrypted += decipher.final("utf8");
    return decrypted;
  } catch (error) {
    console.error("🔐 Decryption error:", error.message);
    throw new Error(
      "Failed to decrypt database connection string. Check your ENCRYPTION_KEY and DB_CNN values."
    );
  }
}

const dbConnection = async () => {
  try {
    console.log("🔗 Attempting to connect to database...");

    // Decrypt the connection string
    const decryptedURI = decryptDBConnectionString();

    // Show masked URI for security in logs
    const maskedURI = decryptedURI.replace(/:([^:]+)@/, ":****@");
    console.log(`📡 Connecting to: ${maskedURI}`);

    // Connect to MongoDB
    await mongoose.connect(decryptedURI, {
      useNewUrlParser: true,
      useUnifiedTopology: true,
      // Add other options as needed
    });

    console.log("✅ DB Connection Successful");

    // Handle connection events
    mongoose.connection.on("error", (err) => {
      console.error("❌ MongoDB connection error:", err);
    });

    mongoose.connection.on("disconnected", () => {
      console.log("⚠️ MongoDB disconnected");
    });

    mongoose.connection.on("reconnected", () => {
      console.log("🔁 MongoDB reconnected");
    });
  } catch (error) {
    console.error("💥 Database connection error:", error.message);
    process.exit(1);
  }
};

export default dbConnection;
