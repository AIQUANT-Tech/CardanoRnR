import nodemailer from 'nodemailer';
import dotenv from 'dotenv';

dotenv.config();


console.log(process.env.HOTEL_EMAIL_ID);
console.log(process.env.HOTEL_EMAIL_ID_PASSWORD);

const transporter = nodemailer.createTransport({
  service: 'gmail',
  port: 465,
  secure: true,
  auth: {
    user: process.env.HOTEL_EMAIL_ID,
    pass: process.env.HOTEL_EMAIL_ID_PASSWORD
  }
});

export const SendRnREmail = async (req, res) => {

  try {
    const { reciepientEmail } = req.body;
    console.log('Reciepient Email:', reciepientEmail);

    if (!reciepientEmail)
    {
      return res.status(400).json({
        success:false,
        message:'Receipent EMAIL is required'
      });
    }

    const mailoptions = {
      from: process.env.HOTEL_EMAIL_ID,
      to: reciepientEmail,
      subject: '🌟 How Was Your Stay at Hotel X?',
      text: `Hi ,\n\nWe hope you enjoyed your stay! Please share your experience:\nhttp://51.21.61.199/user/X}\n\nThank you!\nTeam Hotel X`,
      html: `
        <div style="font-family: Arial, sans-serif; padding: 20px;">
          <p>Hi ,</p>
          
          <p>We hope you enjoyed your stay at <b>Hotel X</b>! 🏨</p>
          
          <p>Please take 1 minute to share your experience:</p>
          
          <div style="margin: 25px 0;">
            <a href="http://51.21.61.199/user/X" 
              style="
                background: #007bff;
                color: white;
                padding: 12px 24px;
                border-radius: 5px;
                text-decoration: none;
                display: inline-block;
              ">
              Click to Review Hotel X
            </a>
          </div>

          <p>Your feedback helps us improve! ⭐<br>
          (Takes less than 60 seconds ⏱️)</p>
          
          <hr style="border: 0.5px solid #eee; margin: 20px 0;">
          
          <p style="color: #666; font-size: 14px;">
            Trouble clicking? Copy this link:<br>
            http://51.21.61.199/user/X
          </p>
        </div>
      `
    };

    const info = await transporter.sendMail(mailoptions);


    res.status(200).json({
      sucess: true,
      message: 'Review email sent succesfully',
      messageId: info.messageId
    });

  }
  catch(error)
  {
    console.error('EMAIL sending error:', error);
    res.status(500).json({
      success: false,
      message: 'Failed why to send review mail',
      error: error.message
    })
    
  };
}






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