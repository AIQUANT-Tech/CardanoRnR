import nodemailer from "nodemailer";
import dotenv from "dotenv";

dotenv.config();

console.log(process.env.HOTEL_EMAIL_ID);
console.log(process.env.HOTEL_EMAIL_ID_PASSWORD);

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
    const { reciepientEmail } = req.body;
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

Review Link: ${process.env.Hotel_Name_Url}

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
      <a href="${process.env.Hotel_Name_Url}"
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
      <a href="${process.env.Hotel_Name_Url}" style="color: #007bff;">
        ${process.env.Hotel_Name_Url}
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

// import nodemailer from 'nodemailer';
// import dotenv from 'dotenv';
// dotenv.config();

// // Verify environment variables first
// console.log('Email:', !!process.env.HOTEL_EMAIL_ID);
// console.log('Password exists:', !!process.env.HOTEL_EMAIL_ID_PASSWORD);

// const transporter = nodemailer.createTransport({
//     host: 'smtp.gmail.com',
//     port: 465,
//     secure: true,
//     auth: {
//         user: process.env.HOTEL_EMAIL_ID,
//         pass: process.env.HOTEL_EMAIL_ID_PASSWORD
//     },
// });

// // Verify transporter configuration
// transporter.verify(function(error, success) {
//     if (error) {
//         console.error('SMTP Connection Failed:', error);
//         console.log('Troubleshooting Tips:');
//         console.log('1. Verify Gmail credentials in .env file');
//         console.log('2. Ensure 2FA is enabled and app password is used');
//         console.log('3. Check Google Account security settings: https://myaccount.google.com/security');
//     } else {
//         console.log('SMTP Connection Ready');
//         sendEmail();
//     }
// });

// function sendEmail() {
//     const mailOptions = {
//         from: process.env.HOTEL_EMAIL_ID,
//         to: process.env.HOTEL_EMAIL_ID,
//         subject: '🌟 How Was Your Stay at Hotel X?',
//         text: `Hi,\n\nWe hope you enjoyed your stay! Please share your experience:\nhttp://51.21.61.199/user/X\n\nThank you!\nTeam Hotel X`,
//         html: `
//             <div style="font-family: Arial, sans-serif; padding: 20px;">
//                 <!-- HTML content unchanged -->
//             </div>
//         `
//     };

//     transporter.sendMail(mailOptions, (error, info) => {
//         if (error) {
//             console.error('Full Error Details:', {
//                 code: error.code,
//                 command: error.command,
//                 response: error.response
//             });
//             console.log('Solution Checklist:');
//             console.log('1. Use app password (not regular password)');
//             console.log('2. Enable IMAP in Gmail settings');
//             console.log('3. Try disabling antivirus/firewall temporarily');
//         } else {
//             console.log('Email successfully sent:', info.response);
//         }
//     });
