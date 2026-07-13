import nodemailer from "nodemailer";
import dotenv from "dotenv";

dotenv.config();

const transporter = nodemailer.createTransport({
  service: "gmail",
  port: 465,
  secure: true,
  auth: {
    user: process.env.HOTEL_EMAIL_ID,
    pass: process.env.HOTEL_EMAIL_ID_PASSWORD,
  },
});

export const SendRnREmail = async (req, res) => {
  try {
    const { reciepientEmail, bookingId } = req.body;
    console.log("Reciepient Email:", reciepientEmail);

    if (!reciepientEmail) {
      return res.status(400).json({
        success: false,
        message: "Receipent EMAIL is required",
      });
    }

    const mailoptions = {
      from: process.env.HOTEL_EMAIL_ID,

      to: reciepientEmail,

      subject:
        "✨ We Hope You Enjoyed Your Stay – Please Share Your Experience!",

      text: `Hello,

Thank you for choosing to stay with Hotel Kimpton Aluna!

We hope you had a comfortable and memorable experience during your stay.

We would truly appreciate it if you could spare a moment to share your feedback with us.

Your review helps us improve and continue delivering the best hospitality experience.

Review Link: ${process.env.Hotel_Name_Url}?bookingId=${bookingId}

Warm regards,
Team Hotel Kimpton Aluna
`,

      html: `
<div style="font-family: 'Segoe UI', Arial, sans-serif; padding: 24px; background: #f7f7f7; color: #333;">
  <div style="max-width: 600px; margin: auto; background: #ffffff; padding: 30px; border-radius: 12px; box-shadow: 0px 3px 10px rgba(0,0,0,0.1);">
    
    <h2 style="color: #222; font-weight: 600; margin-bottom: 12px;">
      ✨ Thank You for Staying With Us!
    </h2>

    <p style="font-size: 15px; line-height: 1.6; margin-bottom: 20px;">
      Hello,<br><br>
      We hope your stay at <b>Hotel Kimpton Aluna</b> was delightful and memorable.
    </p>

    <p style="font-size: 15px; line-height: 1.6; margin-bottom: 15px;">
      Your feedback means a lot to us. It helps us enhance our hospitality and ensure future guests receive the best experience.
    </p>

    <div style="text-align: center; margin: 30px 0;">
      <a href="${process.env.Hotel_Name_Url}?bookingId=${bookingId}"
        style="
          background: #ff9800;
          color: #fff;
          padding: 14px 28px;
          border-radius: 30px;
          font-size: 16px;
          font-weight: bold;
          text-decoration: none;
          display: inline-block;
          box-shadow: 0px 4px 12px rgba(0,0,0,0.15);
        ">
        ⭐ Share Your Stay Experience
      </a>
    </div>

    <p style="font-size: 14px; line-height: 1.6; color: #555;">
      It only takes a minute and helps us improve continuously!
    </p>

    <hr style="border: 0; border-top: 1px solid #eee; margin: 30px 0;" />

    <p style="font-size: 13px; color: #888; line-height: 1.5;">
      If the button above doesn't work, copy and paste this link into your browser: <br>
      <a href="${process.env.Hotel_Name_Url}?bookingId=${bookingId}" style="color: #007bff;">
        ${process.env.Hotel_Name_Url}?bookingId=${bookingId}
      </a>
    </p>

    <p style="font-size: 13px; margin-top: 25px; color: #aaa;">
      Warm regards,<br />
      <b>Team Hotel Kimpton Aluna</b>
    </p>
  </div>
</div>
  `,
    };


    const info = await transporter.sendMail(mailoptions);

    res.status(200).json({
      sucess: true,
      message: "Review email sent succesfully",
      messageId: info.messageId,
    });
  } catch (error) {
    console.error("EMAIL sending error:", error);
    res.status(500).json({
      success: false,
      message: "Failed why to send review mail",
      error: error.message,
    });
  }
};
