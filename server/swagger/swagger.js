import swaggerJsDoc from "swagger-jsdoc";
import swaggerUi from "swagger-ui-express";

const options = {
  definition: {
    openapi: "3.0.0",
    info: {
      title: "RnR API",
      version: "1.0.0",
      description: "API documentation for RnR backend",
    },
    servers: [
      {
        url: process.env.SWAGGER_SERVER_URL,
      },
    ],
  },

  apis: [
    "./reviewCategory/*.js",
    "./review/*.js",
    "./user/*.js",
    "./replyTransactionData/*.js",
    "./cardano_transaction/*.js",
    "./Hotel_Booking_System/*.js",
    "./scheduler/*.js",
    "./Node_Mailer/*.js",
  ],
};

const swaggerSpec = swaggerJsDoc(options);

export function swaggerDocs(app) {
  app.use("/api-docs", swaggerUi.serve, swaggerUi.setup(swaggerSpec));
}
